#!/usr/bin/env lua 
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
-- ## Starting
local l,the,big,help = {},{},1E32,[[

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
  -l leafsize   min leaf size            = 4
  -m m          bayes control            = 2
  -p p          distance coeffecient     = 2
  -r rseed      random seed              = 1234567891
  -s start      init number of samples   = 4
  -S Stop       max number of labellings = 30
  -t train      data                     = ../test/auto93.csv 
  -T Trainings  max size train set       = .33
]]

local SYM, NUM, DATA, COLS, SOME = {}, {}, {}, {}, {}   

local abs, cos, exp, log    = math.abs, math.cos, math.exp, math.log
local max, min, pi, R, sqrt = math.max, math.min, math.pi, math.random, math.sqrt
      
---------------- ----------------- ----------------- ----------------- -----------------
-- ## Data 

-- ### SYM
-- SYMs  summarize a stream of atoms seen in column `at`.

-- `:new(str, int) --> SYM`
function SYM:new(txt,at) 
  return l.new(SYM, {at=at, txt=txt, n=0, has={}, most=0, mode=nil}) end

-- `:add(x) --> nil`      
-- Add an atom to a SYM.
function SYM:add(x)
  if x~="?" then
    self.n = self.n + 1
    self.has[x] = 1 + (self.has[x] or 0) 
    if self.has[x] > self.most then self.most, self.mode = self.has[x], x end end end

-- ### NUM
-- NUMs summarize a stream of numbers seen in column `at`.

-- `:new(str, int) --> NUM`  
-- If a NUM's name end in `-` then its goal is to be minimized (so we set that to `0`). 
function NUM:new(txt, at) 
  return l.new(NUM, {at=at, txt=txt, n=0, mu=0, m2=0, sd=0, lo=big, hi= -big,
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
 
-- ### COLS
-- COLS are factories for generating NUMs or SYMs from a list of column names.

-- `:new(list[str]) --> COLS`   
-- Upper case words become NUMs, others become SYMs. Words ending with `X`
-- are ignored, others get listed in `self.x` (for independent input observatbles)
-- and `self.y` (for dependent goals). Also remember the names as `self.cols` 
-- (which will be used if ever we want to copy the structure of one DATA to another).
function COLS:new(names)
  local all,x,y = {},{},{}
  for at,txt in pairs(names) do 
    l.push(all, (txt:find"^[A-Z]" and NUM or SYM):new(txt,at))
    if not txt:find"X$" then
      l.push(txt:find"[!+-]$" and y or x, all[#all]) end end
  return l.new(COLS, {names=names, all=all, x=x, y=y}) end

-- _:add(list) --> list_    
-- Update the column summaries from `row`.
function COLS:add(row)
  for _,col in pairs(self.all) do col:add(row[col.at]) end 
  return row end

-- ### DATA
-- DATAs store `rows`, summarizes into `col`umns of NUMs or SYMS.

-- `:new() --> DATA`    
function DATA:new()
  return l.new(DATA, {cols=nil, rows={}})  end

-- `:read(str) --> DATA`   
-- Fill in the `rows` of a DATA from a csv `file`.
function DATA:read(file)
  for row in l.csv(file) do self:add(row) end; return self end

-- `:adds(list[list]) --> DATA`   
-- Fill in the `rows` of a DATA from a list or rows.
function DATA:adds(t)
  for _,row in pairs(t or {}) do self:add(row) end; return self end

-- `:add(list) --> DATA`   
-- Add `row` to a DATA. If this is the first row then use it to initialize the columns.
function DATA:add(row) 
  if self.cols then l.push(self.rows,  self.cols:add(row)) else self.cols=COLS:new(row) end 
  return self end 

-- `:clone(list[list]={}) --> DATA`   
-- Return a new DATA with same structure as self. If `rows` is supplied, that add that.
function DATA:clone(rows,     d) 
  return DATA:new():add(self.cols.names):adds(rows) end
   
--------- --------- --------- --------- --------- --------- --------- --------- --------- --------- 
-- ## Dist

-- `:ydist(list) --> 0..1`   
-- Return the distance from a row's goals to best values for each goal.
function DATA:ydist(row, D)
  D = function(y) return (abs(y:norm(row[y.at]) - y.goal))^the.p end
  return (l.sum(self.cols.y,D) /#self.cols.y)^(1/the.p) end

function SYM:dist(a,b)
  return (a=="?" and b=="?" and 0) or (a==b and 0 or 1) end

function NUM:dist(a,b)
  if a=="?" and b=="?" then return 1 end
  a,b = self:norm(a), self:norm(b)
  a = a ~= "?" and a or (b<0.5 and 1 or 0)
  b = b ~= "?" and b or (a<0.5 and 1 or 0)
  return abs(a-b) end

-- Minkowski  distance
function DATA:xdist(row1,row2,    d,n) -- (list) -> number
  D = function(x) return x:dist(row1[x.at], row2[x.at])^the.p  end
  return (l.sum(self.cols.x, D) / #self.cols.x) ^ (1/the.p) end

function DATA:neighbors(row,rows)
  return l.keysort(rows or self.rows, function(r) return self:xdist(r,row) end) end
--------- --------- --------- --------- --------- --------- --------- --------- --------- --------- 
-- ## Bayes

--  `:like(num, num) --> num`   
-- Report how much `x` might belong to a SYMbolic distribution.
function SYM:like(x,prior)
  return  ((self.has[x] or 0) + the.m*prior) / (self.n + the.m)  end

-- `:like(num) --> num`  
-- Report how much `x` might belong to a NUMbolic distribution.
function NUM:like(x,_,      v,tmp)
  v = self.sd^2 + 1/big
  tmp = exp(-1*(x - self.mu)^2/(2*v)) / (2*pi*v) ^ 0.5
  return max(0,min(1, tmp + 1/big)) end

-- `:loglike(list[list], int, int) --> num`   
-- Report the log likelohood that `row` belongs to self (by multiplying the prior
-- by the product of the liklihood of the evidence in `row`).  `nh` is the number of
-- class (including this one) and `nall` is the number of rows in all classes.
function DATA:loglike(row, nall, nh,          prior,F,G)
  prior = (#self.rows + the.k) / (nall + the.k*nh)
  F     = function(x) return L( x:like(row[x.at], prior) ) end
  L     = function(n) return n>0 and log(n) or 0 end
  return L(prior) + l.sum(self.cols.x, F) end

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
  local Y,B,R,BR,test,train,todo,done,best,rest,n,_
  Y  = function(r) return self:ydist(r) end
  B  = function(r) return best:loglike(r, #done, 2) end
  R  = function(r) return rest:loglike(r, #done, 2) end
  BR = function(r) return B(r) - R(r) end
  n  = min(500, the.Trainings * #self.rows)
  train,test = l.split(l.shuffle(self.rows), n)
  test, _    = l.split(test, min(500,#test))
  done,todo  = l.split(train, the.start)            --- [1]
  while true do
    done = l.keysort(done,Y)
    if #done > the.Stop or #todo < 5 then break end --- [6]
    best,rest = l.split(done, sqrt(#done))          --- [2]
    best, rest = self:clone(best), self:clone(rest)  --- [3]
    todo = l.keysort(todo,BR)                       --- [4]
    for _=1,2 do                                    --- [5]
      l.push(done, table.remove(todo)); 
      l.push(done, table.remove(todo,1)) end end
  return done, test, BR end     --- [7]

--------- --------- --------- --------- --------- --------- --------- --------- --------- --------- 
local TREE={}
function TREE:new(data1,data2,  ifTrue,guard0,lvl)
  if #data1.rows + #data2.rows < the.leafsize then return end
  local NEW = function() return data1:clone() end
  local all, ok1, no1, ok2, no2 = NEW(), NEW(), NEW(), NEW(), NEW()
  local guard = data1:guard(data2)
  for _,r in pairs(data1.rows) do all:add(r); (guard:ok(r) and ok1 or no1):add(r)  end
  for _,r in pairs(data2.rows) do all:add(r); (guard:ok(r) and ok2 or no2):add(r)  end
  return l.new(TREE, {lvl    = lvl or 0,
                      here   = all,
                      guard  = guard0,
                      ifTrue = ifTrue,
                      ok     = TREE:new(ok1, ok2, true,  guard, lvl or 1),
                      no     = TREE:new(no1, no2, false, guard, lvl or 1)}) end 

local GUARD={}
function GUARD:new(col,score,lo,hi)
  return l.new(GUARD, {at=col.at, txt=col.txt, lo=lo, hi=hi or lo, score=score}) end

function GUARD:ok(row,      x)
  x = row[self.at]
  return x=="?" and row or self.lo==self.hi and self.lo==x or self.lo <= x and x < self.hi end

function DATA:guard(other,       most,guard,tmp)
  most = 0
	for k,col1 in pairs(self.cols.x) do
    tmp  = col1:guard(other.cols.x[k])
    if tmp.score > most then most,guard = tmp.score,tmp end end
  return guard end

function SYM:guard(other,    most,tmp,it,no)
  most = 0
  for k,ok in pairs(self.count) do
    ok  = ok/self.n
    no  = (other.count[k] or 0)/(other.n + 1E-32)
    tmp = ok - no
    if tmp > most then most,it = tmp,k end end
  return GUARD:new(self, most, it) end

-- Cumulative distribution (area under the pdf up to x).
function NUM:cdf(x,     z,CDF)
  CDF = function(z) return 1 - 0.5*exp(-0.717*z - 0.416*z*z) end
  z = (x - self.mu)/self.sd
  return z >=  0 and CDF(z) or 1 - CDF(-z) end

-- Return value of favoring `i.mu` From
-- [stackoverflow](https://stackoverflow.com/questions/22579434/python-finding-the-intersection-point-of-two-gaussian-curves).
function NUM:guard(other,    i,j,a,b,c, ok,no,x1,x2)
  i,j = self,other
  a  = 1/(2*i.sd^2)  - 1/(2*j.sd^2)  
  b  = j.mu/(j.sd^2) - i.mu/(i.sd^2)
  c  = i.mu^2 /(2*i.sd^2) - j.mu^2 / (2*j.sd^2) - log(j.sd/(i.sd + 1E-32))  
  x1 = (-b - (b^2 - 4*a*c)^.5)/(2*a + 1E-32)
  x2 = (-b + (b^2 - 4*a*c)^.5)/(2*a + 1E-32)
  if x1 > x2 then x1,x2 = x2,x1 end
  ok = i:cdf(x2) - i:cdf(x1)
  no = j:cdf(x2) - j:cdf(x1)
  return GUARD:new(i, ok - no, x1, x2) end

--------- --------- --------- --------- --------- --------- --------- --------- --------- --------- 
-- ## Utilities
-- ###  Lists

-- `l.push(list,atom) --> atom`   
-- Add `atom` to end of `list`.
function l.push(t,x) t[1+#t] = x; return x end

-- `l.split(list, n:int=#list) -> list,list`     
-- Return the first `n` items in one list, then the other items in a second list.
function l.split(t,n)
  local u,v = {},{}
  for j,x in pairs(t) do l.push(j <= n and u or v,x) end
  return u,v end

-- ### Random 

-- `l.any(list) --> Any`  
-- Return any one item from `list`.
function l.any(t)  return t[R(#t)] end

-- `l.many(list, n=#list) --> list`  
-- Return `n` items from `list`.
function l.many(t,n,  u) 
  u={}; for i=1,(n or #t) do u[i] = l.any(t) end; return u end

-- `l.normal(mu:num=0, sd:num) --> num`   
-- Return a value from a normal distribution.
function l.normal(mu,sd) 
   return (mu or 0)+(sd or 1)*sqrt(-2*log(R()))*cos(2*pi*R()) end

-- ### Sorting

-- `l.lt(atom) --> function`    
-- Return a function to sort ascending on key `x`.
function l.lt(x) 
  return function(a,b) return a[x] < b[x] end end

--  `l.shuffle(list) --> list`   
-- Randomly rearrange items in `list`.
function l.shuffle(t,    j)
  for i = #t, 2, -1 do j = R(i); t[i], t[j] = t[j], t[i] end
  return t end

-- `l.sort(list, callable=function(x) return x end) --> list`  
-- Return `list`, sorted by `fn`.
function l.sort(t,fn) table.sort(t,fn); return t end

-- ### Mapping

-- `l.adds(Any, list) --> Any`    
-- Add all items in `list` to `it`.
function l.adds(it,t)
  for _,x in pairs(t or {}) do it:add(x) end; return it end

-- `l.kap(list, callable) --> list`   
-- `kap` calls `fn` with  key and value.
function l.kap(t,fn,    u) --> list
  u={}; for k,v in pairs(t) do u[1+#u]=fn(k,v)  end; return u end

-- `l.kap(list, callable) --> list`     
-- `l.map` calls `fn` with one value.
function l.map(t,fn,     u) --> list
  u={}; for _,v in pairs(t) do u[1+#u] = fn(v)  end; return u end

-- `l.sum(list,callable) --> num`   
-- Return sum of items in `list`, filtered by `fn`.
function l.sum(t,fn,     n)
  n=0; for _,x in pairs(t) do n=n+(fn and fn(x) or x) end; return n end

-- `l.keysort(list, callable) --> list`    
-- The Schwartzian transform is a computer programming technique
-- that uses the decorate-sort-undecorate idiom to improve the efficiency
-- of sorting a list of items.
-- Recommended over `sort` when `callable` is slow to compute.
function l.keysort(t,fn,     DECORATE,UNDECORATE)
  DECORATE   = function(x) return {fn(x),x} end
  UNDECORATE = function(x) return x[2] end
  return l.map(l.sort(l.map(t,DECORATE),l.lt(1)), UNDECORATE) end

-- ### String to thing

-- `l.coerce(str) -> bool | int | float | str`  
-- Convert  a string into some atom.
function l.coerce(s,     F,TRIM) 
  TRIM= function(s) return s:match"^%s*(.-)%s*$" end
  F   = function(s) return s=="true" and true or s ~= "false" and s end
  return math.tointeger(s) or tonumber(s) or F(TRIM(s)) end

-- `l.csv(str) --> Iterator --> list`   
-- Returns an iterator for reading lines in a csv file.
function l.csv(file,     src)
  if file and file ~="-" then src=io.input(file) end
  return function(     s,t)
    s = io.read()
    if   not s 
    then if src then io.close(src) end 
    else t={}; for s1 in s:gmatch"([^,]+)" do t[1+#t]=l.coerce(s1)end; return t end end end

-- `l.datas(list[str]) --> Iterator --> list`  
-- For any string ending in "csv", return that file as a DATA.
function l.datas(t,      i)
  i=0
  return function()
    while i<#t do
      i = i+1
      if t[i]:find"csv" then
        return t[i], DATA:new():read(t[i]) end end end end

-- `l.cli(dict) --> dict`        
-- Update slot xxx in `dict` if there is a command line flag `-x`.
-- For any slot `-x`  with an existing boolean value, then the flag `-x`
-- negates the default.
function l.cli(t)
  for k,v in pairs(t) do
    v = tostring(v)
    for n,x in ipairs(arg) do
      if x=="-"..(k:sub(1,1)) then
        v= v=="false" and "true" or v=="true" and "false" or arg[n+1] end end 
    t[k] = l.coerce(v) end 
  return t end

-- ### Thing to string

-- `l.fmt(str,...) --> str`   
-- Return a string, formatted by fmt.
l.fmt = string.format

-- `l.o(atom | dict | list) --> str`   
-- Generate a string from any atom or nested structure. Skip over
-- private stuff (i.e. anything whose key starts with "_").
function l.o(x,     F,G,GO) --> str
  F  = function() return #x>0 and l.map(x,l.o) or l.sort(l.kap(x,G)) end
  G  = function(k,v) if GO(k) then return l.fmt(":%s %s",k,l.o(x[k])) end end
  GO = function(k,v) return not l.o(k):find"^_" end
  return type(x)=="number" and l.fmt("%g",x) or  
         type(x)~="table"  and tostring(x) or 
         "{" .. table.concat(F()," ") .. "}" end 

-- `l.oo(atom | dict | list) --> nil`     
-- Prints the string generated by `o`.
function l.oo(x) print(l.o(x)) end

-- ### Polymorphism

-- `l.new(list, dict) --> dict`    
-- Return a dictionary that knows where to find its methods.
function l.new(klass, obj)
  klass.__index    = klass
  klass.__tostring = klass.__tostring or l.o
  return setmetatable(obj, klass) end

function l.green(s) return l.fmt('\27[30;42m%s\27[0m',s) end
function l.red(s) return l.fmt('\27[30;41m%s\27[0m',s) end

--------- --------- --------- --------- --------- --------- --------- --------- --------- --------- 
-- ## Stats

function SOME:new(txt,samples,beats)
  return l.new(SOME, {txt=txt, all={}, beats=beats, samples=(samples or 0), num=NUM:new()}) end

function SOME:add(x)
  l.push(self.all, x)
  self.num:add(x) end

function SOME:delta(other, eps,      d)
  d = self.num.mu - other.num.mu
  if abs(d) < (eps or 0) * self.num:pooledSd(other.num) then return 0 end
  if l.cliffs(self.all, other.all) and l.bootstrap(self.all, other.all) then return 0 end
  return d end

-- `:delta(num) -> num`
-- Reports the adjusted mean difference between two NUMs.
function NUM:delta(other)
  return abs(self.mu - other.mu) / ((1E-32 + self.sd^2/self.n + other.sd^2/other.n)^.5) end

-- `:delta(num) -> num`   
-- Reports weighted sum of the standard deviation of two NUMs
function NUM:pooledSd(other)
  return sqrt(((self.n-1)*self.sd^2 + (other.n-1)*other.sd^2)/(self.n+other.n-2)) end 

-- `l.cliffs(list[num], list[num]) --> bool`    
-- Two lists are the same if items from one  fall  towards the middle of the other list.
function l.cliffs(xs,ys)
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
function l.bootstrap(y0,z0)
  local x,y,z,delta0,yhat,zhat,n,this,that,b
  local N= function(t) return l.adds(NUM:new(),t) end
  z,y,x  = N(z0), N(y0), l.adds(N(y0),z0)
  yhat   = l.map(y0, function(y1) return y1 - y.mu + x.mu end)
  zhat   = l.map(z0, function(z1) return z1 - z.mu + x.mu end)
  n,b,delta0 = 0, the.Bootstraps, y:delta(z)
  for i=1, b do 
    if N(l.many(yhat)):delta(N((l.many(zhat)))) > delta0 then n = n + 1 end end
  return n / b >= the.conf end

--------- --------- --------- --------- --------- --------- --------- --------- --------- --------- 
-- ## EG
-- Define some possible start-up actions.

local EG={}

-- All the tests we might run in an order simplest to more complex.
function EG.tests() 
  return l.tests(EG, {"any","sum","split","sort","num","sym","csv","stats"}) end

-- Random selection.
function EG.any(  a)
  a = {10,20,30,40,50,60}
  l.oo(l.map({l.any(a), l.many(a,3),l.shuffle(a),l.keysort(a,function(x) return -x end)},
             l.o)) end

-- Summation.
function EG.sum()
  assert(210 == l.sum{10,20,30,40,50,60}) end

-- Split a list.
function EG.split(    a,b,c)
  a={10,20,30,40,50,60}
  b,c = l.split(a,3)
  print(l.o(b), l.o(c)) end

-- Sort a list.
function EG.sort(    t)
  t={1,2,3,4,5,6,7}
  t=l.sort(t, function(x,y) return  x > y end)
  l.oo{10,4,5}
  l.oo(t) end

-- Example of NUMs.
function EG.num(    n) 
  n = NUM:new()
  for _ = 1,1000 do n:add( l.normal(10,2) ) end
  assert(10-n.mu < 0.1 and 2-n.sd < 0.03) end

-- Example of SYMs.
function EG.sym(    s) 
  s = l.adds(SYM:new(), {"a","a","a","a","b","b","c"})
  print(s.mode, l.o(s.has)) end

-- Show some rows from a csv file.
function EG.csv(   d, n)
  print(the.train)
  n=0
  for row in l.csv(the.train) do n=n+1; if n==1 or n%90==0 then l.oo(row) end end end

-- Compare results from different statistical tests.
function EG.stats(   r,t,u,d,Y,n1,n2)
  Y = function(s) return s and "y" or "." end
  d,r= 1,100
  while d< 1.2 do
    t={}; for i=1,r do t[1+#t] = l.normal(10,2)^2 end 
    u={}; for i,x in pairs(t) do u[i] = x*d end
    d=d*1.01
    n1,n2 = l.adds(NUM:new(),t), l.adds(NUM:new(),u)
    print(
      l.fmt("%.3f\t%s\t%s\t%s", d, Y(l.cliffs(t,u)), Y(l.bootstrap(t,u)), 
                                   Y(abs(n1.mu - n2.mu) < .35*n1:pooledSd(n2)))) end end

-- Fill a DATA from a csv file.
function EG.data(   d)
  d = DATA:new():read(the.train) 
  assert(3184 == (l.sum(d.rows,function(r) return #r end))) end

-- Check the likelihood calculations.
function EG.like(   d,n)
  n,d = 0,DATA:new():read(the.train) 
  for _,row in pairs(l.keysort(d.rows, function(r) return d:loglike(r,#d.rows,1) end)) do 
    n=n+1 
    if n==1 or n % 30==0 then 
      print(l.fmt("%3s : %6.2f : %s",n,d:loglike(row,#d.rows,2), l.o(row))) end end  end

-- One experiment, where we do a guided search of some data.
function EG.acquire(     d,n,trains,tests, model)
  print(the.train)
  d = DATA:new():read(the.train) 
  print(2,#d.rows)
  n = l.adds(NUM:new(), l.map(d.rows, function(r) print(100); return d:ydist(r) end))
  print(1)
  trains,tests, model = d:acquire() 
  --tests = l.keysort(tests, model)
  --print(n.mu,d:ydist(trains[1]), d:ydist(tests[#tests])) 
  end 

-- Another experiment, for multiple command line csv files, for guided search of some data.
function EG.acquires(    r)
  r = 20
  ALL= function(now,b4)
         if not b4 then return now end
         if now:delta(b4) < 0 then now.beats= b4; return now end 
         return b4 end 
  STEP= function(n,d,Y,      here,train,test)
          here = {trains= SOME:new("exploit",n), tests=SOME:new("exploit_test",n)}
          for i = 1,r do
            train,test = d:acquire() 
            here.trains:add(Y(train))
            here.tests:add(Y(test)) end
          return here end 
  for file,d in l.datas(arg) do
    local all,Y
    Y   = function(r) return d:ydist(r) end
    all = {asIs = l.adds(SOME:new("asIs",#d.rows), l.map(d.rows, Y))}
    for _,n in pairs{15,30,60} do
      the.Stop=n
      for k,some in pairs(STEP(n,d,Y)) do 
        all[k] = ALL(some, all[k]) end  end
    print""
    for k,some in pairs(all) do
       print(l.fmt("%5.2f %5s %-20s %s", some.num.mu, some.samples, some.txt,file)) end end end 
    
---------------- ----------------- ----------------- ----------------- -----------------  
-- ## Start-up

-- `l.tests(dict[str,callable], list[str])`   
-- For the tests stored in `eg` run those names in `tests`.
-- Before each one, reset the random number seed to its default.
-- After each one that crashes or returns `false`, add one to `fails` counter.
-- Return the sum of the failures to the operator system.
function l.tests(eg,tests,      FN,_)
  FN = function(x,     ok,msg,bad) 
         math.randomseed(the.rseed)
         ok,msg = xpcall(eg[x], debug.traceback, _)
         bad = ok==false or msg==false
         print((bad and l.red" FAIL " or l.green" PASS ") .." on "..x)
         return bad and 1 or 0 end
  os.exit(l.sum(tests, FN)) end

-- Build the settings from the help string.
help:gsub("%s+-%S%s(%S+)[^=]+=%s+(%S+)%s*\n", function(k,v) the[k]= l.coerce(v) end)

-- If this is the main controlling file, the read the command line and explore the start-up
-- examples.
if l.o(arg):find"kah.lua" then
  the = l.cli(the)
  if the.help then os.exit(print(help)) end
  math.randomseed(the.rseed or 1)
  l.map(arg, function(s) if EG[s:sub(3)] then EG[s:sub(3)]() end end) end

-- Return code which other people can use.
return {SYM=SYM, NUM=NUM, COLS=COLS, DATA=DATA, the=the, help=help, lib=l}
