#!/usr/bin/env lua 
-- <!-- vim : set tabstop=2 shiftwidth=2 expandtab : -->
-- - This Lua script is a program designed for sequential
--   model-based optimization using a Tree-structured Parzen Estimator
--   (TPE) and a Bayesian classifier. It facilitates experimentation
--   with various configuration parameters, making it suitable for tasks
--   involving statistical confidence testing, bootstrap sampling, and
--   classification.
-- - Users can specify a training data file and adjust several experiment
--   settings to fit different data or optimization tasks. The script
--   includes commands for displaying help, setting a random seed, and
--   selecting data partitions for training and testing. 
-- - For a review of the code intelligence, look below for `acquire`.
-- - The code is divided into four sections:
--     - A help string, from which we parse out the settings (into a variable called `the`);
--     - Some general utility functions;
--     - Some classes;
--     - A library of start-up actions called `EG`;
--     - The actual start-up actions.
local the,help = {},[[

kah.lua : how to change your mind (using TPE + Bayes classifier)
(c) 2024 Tim Menzies (timm@ieee.org). MIT license.

USAGE: 
  chmod +x kah.lua
  ./kah.lua [OPTIONS]

OPTIONS:

  -B Bootstraps num.of bootstraps        = 256
  -c conf       statistical confidence   = .05
  -C Cohen      Cohen's threshold        = .35
  -d delta      Cliff's delta            = .195
  -h help       show help                = false
  -k k          bayes control            = 1
  -m m          bayes control            = 2
  -p p          distance coeffecient     = 2
  -r rseed      random seed              = 1234567891
  -s start      init number of samples   = 4
  -S Stop       max number of labellings = 30
  -H Holdout    testing hold our ratio   = .33
  -t train      data                     = ../test/auto93.csv 
]]

local fmt, pop           = string.format, table.remove
local abs, cos, exp, log = math.abs, math.cos, math.exp, math.log
local max, min, pi, sqrt = math.max, math.min, math.pi, math.sqrt
local R                  = math.random

local SYM, NUM, DATA, COLS, SOME = {}, {}, {}, {}, {}
local big = 1E32
      
----------------- ----------------- ----------------- ----------------- ------------------
-- ## Utilities
-- In Lua, things have to be defined before they are used. So utilities
-- come before classes.
-- ###  Lists

-- `push(list,atom) --> atom`   
-- Add `atom` to end of `list`.
local function push(t,x) t[1+#t] = x; return x end

-- `split(list, n:int=#list) -> list,list`     
-- Return the first `n` items in one list, then the other items in a second list.
local function split(t,n)
  local u,v = {},{}
  for j,x in pairs(t) do push(j <= n and u or v,x) end
  return u,v end

-- ### Random 

-- `any(list) --> Any`  
-- Return any one item from `list`.
local function any(t)    
  return t[R(#t)] end

-- `many(list, n=#list) --> list`  
-- Return `n` items from `list`.
local function many(t,n,  u) 
  u={}; for i=1,(n or #t) do u[i] = any(t) end; return u end

-- `normal(mu:num=0, sd:num) --> num`   
-- Return a value from a normal distribution.
local function normal(mu,sd) 
   return (mu or 0)+(sd or 1)*sqrt(-2*log(R()))*cos(2*pi*R()) end

-- ### Sorting

-- `lt(atom) --> function`    
-- Return a function to sort ascending on key `x`.
local function lt(x) 
  return function(a,b) return a[x] < b[x] end end

--  `shuffle(list) --> list`   
-- Randomly rearrange items in `list`.
local function shuffle(t,    j)
  for i = #t, 2, -1 do j = R(i); t[i], t[j] = t[j], t[i] end
  return t end

-- `sort(list, callable=function(x) return x end) --> list`  
-- Return `list`, sorted by `fn`.
local function sort(t,fn) 
  table.sort(t,fn); return t end

-- ### Mapping

-- `adds(Any, list) --> Any`    
-- Add all items in `list` to `it`.
local function adds(it,t)
  for _,x in pairs(t or {}) do it:add(x) end; return it end

-- `kap(list, callable) --> list`   
-- `kap` calls `fn` with  key and value.
local function kap(t,fn,    u) --> list
  u={}; for k,v in pairs(t) do u[1+#u]=fn(k,v)  end; return u end

-- `map(list, callable) --> list`     
-- `kap` calls `fn` with  value.
local function map(t,fn,     u) --> list
  u={}; for _,v in pairs(t) do u[1+#u] = fn(v)  end; return u end

-- `sum(list,callable) --> num`   
-- Return sum of items in `list`, filtered by `fn`.
local function sum(t,fn,     n)
  n=0; for _,x in pairs(t) do n=n+(fn and fn(x) or x) end; return n end

-- `keysort(list, callable) --> list`    
-- The Schwartzian transform is a computer programming technique
-- that uses the decorate-sort-undecorate idiom to improve the efficiency
-- of sorting a list of items.
-- Recommended over `sort` when `callable` is slow to compute.
local function keysort(t,fn,     DECORATE,UNDECORATE)
  DECORATE   = function(x) return {fn(x),x} end
  UNDECORATE = function(x) return x[2] end
  return map(sort(map(t,DECORATE),lt(1)), UNDECORATE) end

-- ### String to thing

-- `coerce(str) -> bool | int | float | str`  
-- Convert  a string into some atom.
local function coerce(s,     F,TRIM) 
  TRIM= function(s) return s:match"^%s*(.-)%s*$" end
  F   = function(s) return s=="true" and true or s ~= "false" and s end
  return math.tointeger(s) or tonumber(s) or F(TRIM(s)) end

-- `csv(str) --> Iterator --> list`   
-- Returns an iterator for reading lines in a csv file.
local function csv(file,     src)
  if file and file ~="-" then src=io.input(file) end
  return function(     s,t)
    s = io.read()
    if   not s 
    then if src then io.close(src) end 
    else t={}; for s1 in s:gmatch"([^,]+)" do push(t,coerce(s1)) end; return t end end end

-- `datas(list[str]) --> Iterator --> list`  
-- For any string ending in "csv", return that file as a DATA.
local function datas(t,      i)
  i=0
  return function()
    while i<#t do
      i = i+1
      if t[i]:find"csv" then
        return t[i], DATA:new():read(t[i]) end end end end

-- ### Thing to string

-- `o(atom | dict | list) --> str`   
-- Generate a string from any atom or nested structure. Skip over
-- private stuff (i.e. anything whose key starts with "_").
local function o(x,     F,G,GO) --> str
  F  = function() return #x>0 and map(x,o) or sort(kap(x,G)) end
  G  = function(k,v) if GO(k) then return fmt(":%s %s",k,o(x[k])) end end
  GO = function(k,v) return not o(k):find"^_" end
  return type(x)=="number" and fmt("%g",x) or  
         type(x)~="table"  and tostring(x) or 
         "{" .. table.concat(F()," ") .. "}" end 

-- `oo(atom | dict | list) --> nil`     
-- Prints the string generated by `o`.
local function oo(x) 
  print(o(x)) end

-- ### Polymorphism

-- `new(list, dict) --> dict`    
-- Return a dictionary that knows where to find its methods.
local function new(klass, obj)
  klass.__index    = klass
  klass.__tostring = klass.__tostring or o
  return setmetatable(obj, klass) end

-- ### Start-up

-- `cli(dict) --> dict`        
-- Update slot xxx in `dict` if there is a command line flag `-x`.
-- For any slot `-x`  with an existing boolean value, then the flag `-x`
-- negates the default.
local function cli(t)
  for k,v in pairs(t) do
    v = tostring(v)
    for n,x in ipairs(arg) do
      if x=="-"..(k:sub(1,1)) then
        v= v=="false" and "true" or v=="true" and "false" or arg[n+1] end end 
    t[k] = coerce(v) end 
  return t end

-- `tests(dict[str,callable], list[str])`   
-- For the tests stored in `eg` run those names in `tests`.
-- Before each one, reset the random number seed to its default.
-- After each one that crashes or returns `false`, add one to `fails` counter.
-- Return the sum of the failures to the operator system.
local function tests(eg,tests,      FN,_)
  FN = function(x,     ok,msg,bad) 
         math.randomseed(the.rseed)
         ok,msg = xpcall(eg[x], debug.traceback, _)
         bad = ok==false or msg==false
         print((bad and "❌" or "✅") .." on "..x)
         return bad and 1 or 0 end
  os.exit(sum(tests, FN)) end

---------------- ----------------- ----------------- ----------------- -----------------
-- ## SYM
-- SYMs  summarize a stream of atoms seen in column `at`.

-- `:new(str, int) --> SYM`
function SYM:new(txt,at) 
  return new(SYM, {at=at, txt=txt, n=0, has={}, most=0, mode=nil}) end

-- `:add(x) --> nil`      
-- Add an atom to a SYM.
function SYM:add(x)
  if x~="?" then
    self.n = self.n + 1
    self.has[x] = 1 + (self.has[x] or 0) 
    if self.has[x] > self.most then self.most, self.mode = self.has[x], x end end end

--  `:like(num, num) --> num`   
-- Report how much `x` might belong to a SYMbolic distribution.
function SYM:like(x,prior)
  return  ((self.has[x] or 0) + the.m*prior) / (self.n + the.m)  end

----------------- ----------------- ----------------- ----------------- -----------------  
-- ## NUM
-- NUMs summarize a stream of numbers seen in column `at`.

-- `:new(str, int) --> NUM`  
-- If a NUM's name end in `-` then its goal is to be minimized (so we set that to `0`). 
function NUM:new(txt, at) 
  return new(NUM, {at=at, txt=txt, n=0, mu=0, m2=0, sd=0, lo=big, hi= -big,
                   goal = (txt or ""):find"-$" and 0 or 1}) end

-- `:add(num) --> nil`  
-- Updates a NUM with `x` using the 
-- [Welford on-line](https://en.wikipedia.org/wiki/Algorithms_for_calculating_variance#Welford's_online_algorithm).
-- algorithm.
function NUM:add(x)
  if x~="?" then
    self.n = self.n + 1
    local d = x - self.mu
    self.mu = self.mu + d / self.n
    self.m2 = self.m2 + d * (x - self.mu)
    self.sd = self.n < 2 and 0 or (self.m2/(self.n - 1))^.5
    if x > self.hi then self.hi = x end
    if x < self.lo then self.lo = x end end end

-- `:like(num) --> num`  
-- Report how much `x` might belong to a NUMbolic distribution.
function NUM:like(x,_,      v,tmp)
  v = self.sd^2 + 1/big
  tmp = exp(-1*(x - self.mu)^2/(2*v)) / (2*pi*v) ^ 0.5
  return max(0,min(1, tmp + 1/big)) end
 
-- `:norm(num) --> 0..1`  
-- Map `x` to the range 0..1 for `lo`..`hi`.
function NUM:norm(x)
  return x=="?" and x or (x - self.lo)/(self.hi - self.lo) end
       
-- `:delta(num) -> num`   
-- Reports the adjusted mean difference between two NUMs.
function NUM:delta(other)
  return abs(self.mu - other.mu) / ((1E-32 + self.sd^2/self.n + other.sd^2/other.n)^.5) end
 
function NUM:pooledSd(other)
  return sqrt(((self.n-1)*self.sd^2 + (other.n-1)*other.sd^2) / (self.n + other.n-2)) end

----------------- ----------------- ----------------- ----------------- -----------------  
-- ## COLS
-- COLS are factories for generating NUMs or SYMs from a list of column names.

-- `:new(list[str]) --> COLS`   
-- Upper case words become NUMs, others become SYMs. Words ending with `X`
-- are ignored, others get listed in `self.x` (for independent input observatbles)
-- and `self.y` (for dependent goals). Also remember the names as `self.cols` 
-- (which will be used if ever we want to copy the structure of one DATA to another).
function COLS:new(names)
  local all,x,y = {},{},{}
  for at,txt in pairs(names) do 
    push(all, (txt:find"^[A-Z]" and NUM or SYM):new(txt,at))
    if not txt:find"X$" then
      push(txt:find"[!+-]$" and y or x, all[#all]) end end
  return new(COLS, {names=names, all=all, x=x, y=y}) end

-- _:add(list) --> list_    
-- Update the column summaries from `row`.
function COLS:add(row)
  for _,col in pairs(self.all) do col:add(row[col.at]) end 
  return row end

----------------- ----------------- ----------------- ----------------- -----------------  
-- ## DATA
-- DATAs store `rows`, summarizes into `col`umns of NUMs or SYMS.

-- `:new() --> DATA`    
function DATA:new()
  return new(DATA, {cols=nil, rows={}})  end

-- `:read(str) --> DATA`   
-- Fill in the `rows` of a DATA from a csv `file`.
function DATA:read(file)
  for row in csv(file) do self:add(row) end; return self end

-- `:adds(list[list]) --> DATA`   
-- Fill in the `rows` of a DATA from a list or rows.
function DATA:adds(t)
  for row in pairs(t or {}) do self:add(row) end; return self end

-- `:add(list) --> DATA`   
-- Add `row` to a DATA. If this is the first row then use it to initialize the columns.
function DATA:add(row) 
  if self.cols then push(self.rows,  self.cols:add(row)) else self.cols=COLS:new(row) end 
  return self end 

-- `:clone(list[list]={}) --> DATA`   
-- Return a new DATA with same structure as self. If `rows` is supplied, that add that.
function DATA:clone(rows) 
  return DATA:new():add(self.cols.names):adds(rows) end
   
-- `:loglike(list[list], int, int) --> num`   
-- Report the log likelohood that `row` belongs to self (by multiplying the prior
-- by the product of the liklihood of the evidence in `row`).  `nh` is the number of
-- class (including this one) and `nall` is the number of rows in all classes.
function DATA:loglike(row, nall, nh,          prior,F,G)
  prior = (#self.rows + the.k) / (nall + the.k*nh)
  F     = function(x) return L( x:like(row[x.at], prior) ) end
  L     = function(n) return n>0 and log(n) or 0 end
  return L(prior) + sum(self.cols.x, F) end

-- `:ydist(list) --> 0..1`   
-- Return the distance from a row's goals to best values for each goal.
function DATA:ydist(row, D)
  D = function(y) return (abs(y:norm(row[y.at]) - y.goal))^the.p end
  return (sum(self.cols.y,D) /#self.cols.y)^(1/the.p) end

-- `DATA:active() --> list,list`  
--      
-- 1. Sort a few labelled few examples. 
-- 2. split them  into best and rest.
-- 3. Use that split to  build a two-class classifier. 
-- 4. Use that classifier to sort the unlabelled
--    examples by their likelihood of belong to best, not rest. 
-- 5. Label the first and last items in that sort.
-- 6. If can you label more items, then go to 2. Else...
-- 7. ... use the classifier to sort the remaining
--    unlabelled examples. Repor the best in that test set.
function DATA:acquire()
  local Y,B,R,BR,test,train,todo,done,best,rest
  Y  = function(r) return self:ydist(r) end
  B  = function(r) return best:loglike(r, #done, 2) end
  R  = function(r) return rest:loglike(r, #done, 2) end
  BR = function(r) return B(r) - R(r) end
  test,train = split(shuffle(self.rows), the.Holdout * #self.rows)
  done,todo  = split(train, the.start)              --- [1]
  while true do
    done = keysort(done,Y)
    if #done > the.Stop or #todo < 5 then break end --- [6]
    best,rest = split(done, sqrt(#done))            --- [2]
    best,rest = self:clone(best), self:clone(rest)  --- [3]
    todo = keysort(todo,BR)                         --- [4]
    for _=1,2 do                                    --- [5]
      push(done, pop(todo)); 
      push(done, pop(todo,1)) end end
  return done[1], keysort(test,BR)[#test] end       --- [7]

----------------- ----------------- ----------------- ----------------- -----------------  
-- ## Stats

-- `cliffs(list[num], list[num]) --> bool`    
-- Two lists are the same if items from one  fall  towards the middle of the other list.
local function cliffs(xs,ys)
  local lt,gt,n = 0,0,0
  for _,x in pairs(xs) do
     for _,y in pairs(ys) do
       n = n + 1
       if y > x then gt = gt + 1 end
       if y < x then lt = lt + 1 end end end
  return abs(gt - lt)/n <= the.delta end -- 0.195 

-- `boostrap(list[num], list[num]) --> bool`   
-- `Delta0` is an observation that compute an observation between two  lists.
-- Then we compare `delta0` to observations seen in hundreds of other random 
-- samples from those lists. Two distributions are the same if we can't tell a 
-- difference in those observations.
-- Taken from non-parametric significance test From Introduction to Bootstrap,
-- Efron and Tibshirani, 1993, chapter 20. https://doi.org/10.1201/9780429246593
local function bootstrap(y0,z0)
  local x,y,z,delta0,yhat,zhat,n,this,that,b
  z,y,x  = adds(NUM:new(),z0), adds(NUM:new(),y0), adds(adds(NUM:new(),y0),z0)
  delta0 = y:delta(z)
  yhat   = map(y0, function(y1) return y1 - y.mu + x.mu end)
  zhat   = map(z0, function(z1) return z1 - z.mu + x.mu end)
  n,b    = 0, the.Bootstraps
  for i=1, b do
    this, that = adds(NUM:new(),many(yhat)), adds(NUM:new(),many(zhat))
    n = n + (this:delta(that) > delta0 and 1 or 0) end
  return n / b >= the.conf end

function SOME:new(txt,sample,beats)
  return new(SOME, {txt=txt, all={}, beats=beats, sample=(sample or 0), num=NUM:new()}) end

function SOME:add(x)
  push(self.all, x)
  self.num:add(x) end

function SOME:delta(other, eps, base,      sd)
  sd = self.num:pooled(other.num) 
  if abs(self.mu - other.mu) < sd *(eps or 0) then return 0 end
  if cliffs(self.all,other.all) and bootstrap(self.all, other.all) then return 0 end
  return (self.mu - other.mu)/(base or 1) end
---------------- ----------------- ----------------- ----------------- -----------------  
-- ## EG
-- Store start-up actions.

local EG={}

-- All the tests we might run in an order simplest to more complex.
function EG.all() 
  return tests(EG, {"any","sum","split","sort","num","sym","csv"}) end

-- Random selection.
function EG.any(  a)
  a = {10,20,30,40,50,60}
  oo(map({any(a), many(a,3),shuffle(a),keysort(a,function(x) return -x end)},
     o)) end 

-- Summation.
function EG.sum()
  assert(210 == sum{10,20,30,40,50,60}) end

-- Split a list.
function EG.split(    a,b,c)
  a={10,20,30,40,50,60}
  b,c = split(a,3)
  print(o(b), o(c)) end

-- Sort a list.
function EG.sort(    t)
  t={1,2,3,4,5,6,7}
  t=sort(t, function(x,y) return  x > y end)
  oo{10,4,5}
  oo(t) end

-- Example of NUMs.
function EG.num(    n) 
  n = NUM:new()
  for _ = 1,1000 do n:add( normal(10,2) ) end
  assert(10-n.mu < 0.1 and 2-n.sd < 0.03) end

-- Example of SYMs.
function EG.sym(    s) 
  s = adds(SYM:new(), {"a","a","a","a","b","b","c"})
  print(s.mode, o(s.has)) end

-- Show some rows from a csv file.
function EG.csv(   d, n)
  print(the.train)
  n=0
  for row in csv(the.train) do n=n+1 ; if n==1 or n % 90==0 then oo(row) end end end

-- Compare results from different statistical tests.
function EG.stats(   r,t,u,d)
  d,r= 1,100
  while d< 1.2 do
    t={}; for i=1,r do t[1+#t] = normal(10,2)^2 end 
    u={}; for i,x in pairs(t) do u[i] = x*d end
    d=d*1.01
    print(fmt("%.3f    %s %s",d, cliffs(t,u) and "y" or ".", bootstrap(t,u) and "y" or "."))
  end end

-- Fill a DATA from a csv file.
function EG.data(   d)
  d = DATA:new():read(the.train) 
  assert(3184 == (sum(d.rows,function(r) return #r end))) end

-- Check the likelihood calculations.
function EG.like(   d,n)
  n,d = 0,DATA:new():read(the.train) 
  for _,row in pairs(d.rows) do 
    n=n+1 
    if n==1 or n % 15==0 then 
      print(fmt("%.3f %s",d:loglike(row,#d.rows,2), o(row))) end end  end

-- One experiment, where we do a guided search of some data.
function EG.acquire(     d, train,test)
  d = DATA:new():read(the.train) 
  train,test = d:acquire() 
  print(d:ydist(train), d:ydist(test)) end

-- Another experiment, for multiple command line csv files, for guided search of some data.
function EG.acquirea(     d,y,trains,tests,train,test,r,asIs,num0,num1,num2,eps,diff)
  r = 20
  for file,d in datas(arg) do
      Y= function(r) return d:ydist(r) end
      asIs = adds(NUM:new(), map(d.rows, Y))
      for _,n in pairs{15,30,50,80,120} do
        the.Stop=n
        trains,tests = {},{}
        for i=1, r do
          train,test = d:acquire() 
          push(trains,Y(train)) 
          push(tests, Y(test)) end
        num0=adds(NUM:new(), asIs)
        num1=adds(NUM:new(), trains)
        num2=adds(NUM:new(), tests)
        eps = num0.sd *.35
        diff=num1.mu - num2.mu
        oo{file=file:gsub(".*/",""), n=the.Stop, eps=eps,mu0=asIs.mu, mu1=num1.mu, mu2=num2.mu,
           delta = same(train,tests) and 0 or abs(diff) < eps and 0 or diff} end end end 
      
----------------- ----------------- ----------------- ----------------- -----------------  
-- ## Start-up

-- Build the settings from the help string.
help:gsub("%s+-%S%s(%S+)[^=]+=%s+(%S+)%s*\n", function(k,v) the[k]= coerce(v) end)

-- If this is the main controlling file, the read the command line and explore the start-up
-- examples.
if o(arg):find"kah.lua" then
   the = cli(the)
   if the.help then os.exit(print(help)) end
   math.randomseed(the.rseed or 1)
   map(arg, function(s) if EG[s:sub(3)] then EG[s:sub(3)]() end end) end

-- Return code which other people can use.
return {SYM=SYM, NUM=NUM, COLS=COLS, DATA=DATA, the=the, help=help}
