local the,go={},{}
go["-h"]= function(_) print(string.format([[

kah.lua : peek at a few rows, find a near global best
(c) 2024 Tim Menzies <timm@ieee.org>, MIT license.

USAGE:
  lua kah.lua [OPTIONS] [DEMO]

OPTIONS:
  -h       print help
  -d file  csv file of data (default: %s)
  -p int   coefficient of distance (default: %s)
  -r int   random number seed  (default: %s)
  -s int   #samples searched for each new centroid (default: %s)

DEMO:
  --header          header generation
  --csv    [file]   csv reader
  --data   [file]   loading rows
  --x      [file]   sorting rows by x values
  --y      [file]   sorting rows by y values
  --around [file]   find very good examples via kmeans++ initialization

]], the.data, the.p, the.rseed, the.samples)) end

-- ## Config 
the= {p= 2,
      data= "../../moot/optimize/misc/auto93.csv",
      rseed= 1234567891,
      samples= 32}

local Big=1E32

go["-p"]= function(s) the.p = s+0 end
go["-d"]= function(s) the.data = s end
go["-s"]= function(s) the.samples = s+0 end
go["-r"]= function(s) the.rseed = s+0; math.randomseed(the.rseed) end

-- -----------------------------------------------------------------------------
-- ### Structs
local Num, Sym, Data, Cols, Sample = {},{},{},{},{}
local new,push

function Sym:new(s,n) -- Summarize numeric columns
  return new(Sym,{
      txt = s,        -- text about this column
      pos = n or 0,   -- column number
      n = 0,          -- how many items?
      has = {},       -- Symbol counts seen so far
      mode = nil,     -- most common symbol
      most = 0 }) end -- frequency of most common symbol

function Num:new(s,n) -- Summarize numeric columns
  return new(Num,{
      txt = s,         -- text about this column
      pos = n or 0 ,   -- column number
      n = 0,          -- how many items?
      lo= Big,        -- smallest number seen in a column
      hi= -Big,       -- largest number seen in a column
      utopia = (s or ""):find"-$" and 0 or 1, -- (min|max)imize = 0,1
      mu= 0,          -- mean
      m2= 0,          -- second moment (used for sd calculation)
      sd= 0 }) end    -- standard deviation

function Data:new(src) -- Holds the rows and column summaries.
  self = new(Data, {cols = Cols:new(src()), -- column information
                    rows={} })             -- set of rows
  return self:adds(src) end

function Cols:new(ss) -- Make and store Nums and Syms
  return new(Cols, {
      names = ss,     -- all the names
      klass = nil,   -- klass column, if it exists
      all   = {},     -- all the cols
      x     = {},     -- the independent columns
      y     = {}}    -- the dependent columns
      ):initialize(ss) end

function Cols:initialize(ss)--> Cols, col names in `ss` turned to Nums or Syms
  for n,s in pairs(ss) do
    local col = (s:find"^[A-Z]" and Num or Sym):new(s,n) -- make Nums or Syms
    push(self.all, col)                                  -- put `col` in `all`
    if not s:find"X$" then                               -- ignore some cols
      push(s:find"[!+-]$" and self.y or self.x,  col)    -- keep in `x` or `y`
      if s:find"!$" then self.klass = col end end end    -- keep the klass
  return self end 

function Sample:new(s) 
  return new(Sample,{txt=s, n=0, mu=0, m2=0, sd=0, lo=Big, hi=-Big, all={}}) end

-- -----------------------------------------------------------------------------
-- ## Code Conventions
-- 
-- - Constructors are functions with Upper case names; e.g. Num()
-- - Optional args denoted by 2 blanks
-- - Local variables denotes by 4 spaces.
-- - In function argument lists, for non-local vars
--   - n=num, s=string, b=bool,a=array,f=fun, x=anything;   
--     e.g. sfile is a string that is a file name
--   - u is an output array.
--   - d=dict,t=a or d,
--   - ns,ss,bs = array of num,s,bs
--   - nums,cols,datas = array of Num, Cols, Datas
-- - Often, 
--   - i,j,are loop counters
--   - k,v are key value pairs from dict.
--   - e,g,h,m,p,q,r,u,w,x,y are anything at all
--   - z is a variable catching the output
--
-- ## References
--
-- [1] https://en.wikipedia.org/wiki/Fisher-Yates_shuffle
-- [2] https://link.springer.com/article/10.1007/BF00153759, section 2.4
-- [3] http://tiny.cc/welford, 
-- [4] chapter 20. https://doi.org/10.1201/9780429246593
-- [5] https://journals.sagepub.com/doi/pdf/10.3102/10769986025002101, table1

-------------------------------------------------------------------------------
--             
--  |   o  |_  
--  |_  |  |_) 

-- ### Lists
function push(a,x)--> x,  added to end of `a`.
  a[1+#a] =x; return x end

local function any(a)--> x (any items of `a`)
  return a[math.random(#a)] end

local function many(a,n,   z)--> a (`n` random items from `a`).
  z={}; for i = 1,(n or #a) do z[i]=any(a) end; return z end

local function items(a,    i)--> f.  Iterator for arrays.
  i=0; return function() i=i+1; if i <= #a then return a[i] end end end

-- ### Sort
local function two(f)--> f, sorted by `f(x) < f(y)`.
  return function(p,q) return f(p) < f(q) end end

local function lt(x)--> f, sorted by  `p[s] < q[s]`.  
  return function(p,q) return p[x] < q[x] end end

local function gt(x)--> f,  sorted by  `a1[s] > a2[s]`.  
  return function(a1,a2) return a1[x] > a2[x] end end

local function sort(a,f)--> a, sorted via `f`. 
  table.sort(a,f); return a end

local function shuffle(a,    j)--> a, randomly re-ordered via Fisher-Yates [1]. 
  for i= #a,2,-1 do j= math.random(i); a[i], a[j] = a[j], a[i] end; return a end

-- ### Map
local function map(a,f,     z)--> a. Return items in `a` filtered through `f`.
  z={}; for _,x in pairs(a) do z[1+#z]=f(x) end ; return z end

local function min(a,f,      n,lo,z)--> x (item in `a` that scores least on `f`).
  lo = math.huge
  for _,x in pairs(a) do
    n = f(x)
    if n < lo then lo,z = n,x end end
  return z end

-- local function pick(d,     u,r,all,anything)--> x (a key in `d`) bias by `d`'s vals
--   u,all={},0
--   for x,n in pairs(d) do push(u,{n,x}); all= all + n end
--   r = math.random()
--   for _,xn in pairs(sort(u,gt(1))) do
--     r = r - xn[1]/all
--     if r <= 0 then return xn[2] else anything = anything or xn[2] end end
--   return anything end
-- 
-- ## Strings to Things (and back again)
local fmt=string.format

local function yellow(s) return "\27[33m" .. s .. "\27[0m" end
local function green(s)  return "\27[32m" .. s .. "\27[0m" end
local function red(s)    return "\27[31m" .. s .. "\27[0m" end

local function csv(sFile,     f, src)--> f (iterator for csv rows)
  f = function(s2,z)
        for s3 in s2:gmatch"([^,]+)" do z[1+#z]=s3:match"^%s*(.-)%s*$" end
        return z end
  src = io.input(sFile)
  return function(      s1)
    s1 = io.read()
    if s1 then return f(s1,{}) else io.close(src) end end  end

local function o(x,          t,LIST,DICT)--> s. Generate a string for `x`.
  LIST= function() for k,v in pairs(x) do t[1+#t]= o(v) end end
  DICT= function() for k,v in pairs(x) do t[1+#t]= fmt(":%s %s",k,o(v)) end end
  t   = {}
  if type(x) == "number" then return fmt(x//1 == x and "%s" or "%.3g",x) end
  if type(x) ~= "table"  then return tostring(x) end
  if #x>0 then LIST() else DICT(); table.sort(t) end
  return "{" .. table.concat(t, " ") .. "}" end

-- ### Polymorphism
function new(methods, a)--a, attached to a delegation table of `methods`.
  methods.__index = methods
  methods.__tostring = methods.__tostring or o
  return setmetatable(a,methods) end

--------------------------------------------------------------------------------
--  | |  ._    _|   _.  _|_   _  
--  |_|  |_)  (_|  (_|   |_  (/_ 
--       |                       

function Sym:add(x)--> x. Updates Sym
  if x=="?" then return x end
  self.n = self.n + 1
  self.has[x] = 1 + (self.has[x] or 0)
  if self.has[x] > self.most then
    self.most, self.mode = self.has[x], x end 
  return x end

function Num:add(n)--> n. Updates Num using Welford's algorithm [3].
  if n=="?" then return n end
  self.n  = self.n + 1
  n       = n + 0 -- ensure we have numbers
  local delta = n - self.mu
  self.mu = self.mu + delta/self.n
  self.m2 = self.m2 + delta*(n - self.mu)
  self.sd = self.n < 2 and 0 or (self.m2/(self.n - 1))^0.5
  self.lo = math.min(n, self.lo)
  self.hi = math.max(n, self.hi)
  return n end

function Data:add(a)--> Data,  updated with one row.
  for _,col in pairs(self.cols.all) do a[col.pos] = col:add(a[col.pos]) end 
  push(self.rows, a)
  return self end

function Data:adds(src)--> Data, updated with many rows
  for a in src  do self:add(a) end
  return self end

-------------------------------------------------------------------------------
--   _                                 
--  | \  o   _  _|_   _.  ._    _   _  
--  |_/  |  _>   |_  (_|  | |  (_  (/_ 

function Num:normalize(n)--> 0...1
  return n=="?" and n or (n - self.lo) / (self.hi - self.lo) end

function Num:dist(p,q)--> n. Distance between two numbers.
  if (p=="?" and q=="?") then return 1 end -- if all unknown, assume max
  p,q = self:normalize(p), self:normalize(q)
  p = p ~= "?" and p or (q<0.5 and 1 or 0) -- when in doubt, assume max
  q = q ~= "?" and q or (p<0.5 and 1 or 0) -- when in doubt, assume max
  return math.abs(p - q) end

function Sym:dist(p,q)--> n. Distance between two symbols.
  if p=="?" and q=="?" then return 1 end -- if all unknown, assume max
  return p==q and 0 or 1 end

function Data:xdist(row1,row2,    d,f)--> n. Gap between x cols of rows [2].
  d,f = 0,function(col) return col:dist(row1[col.pos], row2[col.pos]) end
  for _,col in pairs(self.cols.x) do d = d + math.abs(f(col)) ^ the.p end
  return (d/#self.cols.x) ^ (1/the.p) end 

function Data:ydist(row,    d,f)--> n. Distance of y cols to utopia points.
  d,f = 0,function(col) return col:normalize(row[col.pos]) - col.utopia end
  for _,col in pairs(self.cols.y) do d = d + math.abs(f(col)) ^ the.p end
  return (d/#self.cols.y) ^ (1/the.p) end

-- kmeans++ initialization. Find  centroids are distance^2 from existing ones.
function Data:around(k,  rows,      z)--> rows
  rows = rows or self.rows
  z = {any(rows)}
  for _ = 2,k do 
    local all,u = 0,{}
    for _ = 1,math.min(the.samples, #rows) do
      local row = any(rows)
      local closest = min(z, function(maybe) return self:xdist(row,maybe) end) 
      if row and closest then
         all = all + push(u,{row=row, d=self:xdist(row,closest)^2}).d end
    end
    local r = math.random()
    for _,x in pairs(u) do
      r = r - x.d / all
      if r <= 0 then push(z, x.row); break end end end
  return z end

--------------------------------------------------------------------------------
--   __                    
--  (_   _|_   _.  _|_   _ 
--  __)   |_  (_|   |_  _> 

local function adds(ns,     s)--> Sample. Load numbers in `ns` into a Sample.
  s=Sample(); for _,n in pairs(ns) do s:add(n) end; return s end

-- Checks how rare are  the observed differences between samples of this data.
-- If not rare, then these sets are the same. From [4]
local function boot(y0,z0,  straps,conf,     x,y,z,yhat,zhat,n)
  z,y,x = adds(z0), adds(y0), adds(y0, adds(z0))
  yhat  = map(y0, function(y1) return y1 - y.mu + x.mu end)
  zhat  = map(z0, function(z1) return z1 - z.mu + x.mu end)
  n     = 0 
  for _ = 1,(straps or 512)  do
    if adds(many(yhat)):delta(adds(many(zhat))) > y:delta(z) 
    then n=n+1 end end
  return n / (straps or 512) >= (conf or 0.05)  end

-- How central are `ys` items in `xs`? If central, then the sets are the  same.
local function cliffs(xs,ys,  delta,      lt,gt,n)--> b. [5]
  lt,gt,n,delta = 0,0,0,delta or 0.197 
  for _,x in pairs(xs) do
      for _,y in pairs(ys) do
        n = n + 1
        if y > x then gt = gt + 1 end
        if y < x then lt = lt + 1 end end end
  return math.abs(gt - lt)/n <= delta end -- 0.195 

function Sample:add(n,    d)--> n. Update a Sample with `n`.
  if n=="?" then return n end
  self.n  = self.n + 1
  n       = n + 0 -- ensure we have numbers
  local delta = n - self.mu
  self.mu = self.mu + delta / self.n
  self.m2 = self.m2 + delta * (n - self.mu)
  self.sd = self.n < 2 and 0 or (self.m2/(self.n - 1))^.5  
  self.lo = math.min(n, self.lo)
  self.hi = math.max(n, self.hi)
  self.all[1+#self.all] = n 
  return n end

function Sample.delta(i,j)--> n. Report mean difference, normalized by sd.
  return math.abs(i.mu - j.mu) / ((1E-32 + i.sd^2/i.n + j.sd^2/j.n)^.5) end

function Sample:cohen(other,  d,     sd,i,j)--> b. Parametric effect size.
  i,j = self,other
  sd = (((i.n-1) * i.sd^2 + (j.n-1) * j.sd^2) / (i.n+j.n-2))^0.5
  return math.abs(i.mu - j.mu) <= (d or 0.35) * sd end

function Sample:same(other,  delta,straps,conf,i,j)--> b. Non parametric tests
  i,j = self,other
  return cliffs(i.all,j.all,delta) and boot(i.all,j.all,straps,conf) end

function Sample:__tostring() --> s. Reports some details of Samples.
  return fmt("Sample{%s %g %g %g}",self.txt,self.n,self.mu,self.sd) end

function Sample:merge(other,eps,      i,j,k)
  i,j = self,other
  if math.abs(i.mu - j.mu) < (eps or 0.01) or i:same(j) then
    k = Sample:new(i.txt)
    for _,t in pairs{i.all, j.all} do
      for _,x in pairs(t) do k:add(x) end end  end
  return k end 

function Sample.merges(samples,eps,     pos,t,merged,sample)
  pos={}
  for k,sample in pairs(samples) do
    if t 
    then merged = sample:merge(t[#t],eps)
         if merged then t[#t] = merged else push(t,sample) end
    else t={sample} end
    pos[k] = {sample,#t} end
  for _,two in pairs(pos) do 
	   sample = two[1]
     sample._meta = t[two[2]]
     sample._meta.rank  = fmt("%c",96 + two[2]) end
  return samples end

local function normal(mu,sd,    r)
  return (mu or 0) + (sd or 1) * math.sqrt(-2*math.log(math.random()))
                                 * math.cos(2*math.pi*math.random()) end

-------------------------------------------------------------------------------
--  ___                                       
--   |    _    _  _|_     _   _.   _   _    _ 
--   |   (/_  _>   |_    (_  (_|  _>  (/_  _> 
                                           
local function run(ss, funs, rSeed,       ok,msg,fails)
  fails = 0
  for _,one in pairs(ss) do
    math.randomseed(rSeed or 1234567891)
    ok,msg = xpcall(funs[one], debug.traceback)
    if   ok == false 
    then print(red("FAILURE for '"..one.."' :"..msg)); fails=fails + 1
    else print(green("pass for '"..one.."'")) end 
  end 
  print(yellow(fmt("%s failure(s)",fails)))
  os.exit(fails) end

-- e.g. command line option `lua kseed.lua --the` calls `go["--the"]()`
go["--the"] = function(_)  print(o(the)) end

go["--header"] = function(_,      data)
  data = Data:new(items({{"name","Age","Shoesize-"}}))
  for k,col in pairs(data.cols.x) do print("x",k,col) end 
  for k,col in pairs(data.cols.y) do print("y",k,col) end end

go["--csv"] = function(file,    data,k)
  k=0
  for row in csv(file or the.data) do
    k=k+1
    if k==1 or k%30==0 then print(o(row)) end end end

go["--data"] = function(file,   data) 
  data= Data:new(csv(file or the.data))
  print(#data.rows, o(data.cols.y[1])) end

go["--xs"] = function(file,    data,X,XX)
  data= Data:new(csv(file or the.data))
  X = function(row) return data:xdist(data.rows[1], row) end
  XX= function(a,b) return X(a) < X(b) end
  for k,row in pairs(sort(data.rows,XX)) do
    if k==1 or k% 30==0 then print(o(row), X(row)) end end end

go["--ys"] = function(file,    data,Y,YY)
  data= Data:new(csv(file or the.data))
  Y = function(row) return data:ydist(row) end
  YY= function(a,b) return Y(a) < Y(b) end
  for k,row in pairs(sort(data.rows,YY)) do
    if k==1 or k% 30==0 then print(o(row), Y(row)) end end end

go["--around"] = function(file,     data,Y)
  data= Data:new(csv(file or the.data))
  Y = function(row) return data:ydist(row) end
  for _=1,20 do
    shuffle(data.rows)
    print(Y(sort(data:around(20),two(Y))[1])) end end

go["--stats1"] =function(_,s,r)
  r=5
	print("r",fmt("\tmu\t\tsd")) 
	while r < 50000 do
	  r=r*2
	  s=Sample:new()
	  for i=1,r do s:add(normal(100,20)) end
	  print(r,fmt("\t%g\t\t%g",s.mu, s.sd)) end end

go["--stats2"]=function(_,dot,t,u)
  dot = function(s) return s and "y" or "." end
  print("d\tclif\tboot\tsame\tcohen")
  for d =1,1.25,0.02 do
     t = Sample:new(); for i=1,50 do t:add( normal(5,1) + normal(10,2)^2) end
     u = Sample:new(); for _,x in pairs(t.all) do u:add( x*d) end
     print(fmt("%.3f\t%s\t%s\t%s\t%s",
                         d,dot(cliffs(t.all,u.all)),dot(boot(t.all,u.all)), 
                           dot(t:same(u)), dot(t:cohen(u)))) end  end

go["--compare"] = function(file)
  local SORTER,Y,X,G,G0,all,b4,copy,repeats,data,all,first,want,rand,u,report
  SORTER=function(a,b)
           return a._meta.mu < b._meta.mu or
                 (a._meta.mu == b._meta.mu and a.txt < b.txt) end
  G = function(x) return fmt("%.2f",x) end
  G0 = function(x) return 100*x//1 end
  repeats=50
  data= Data:new(csv(file or the.data))
  Y = function(row) return data:ydist(row) end
  b4=Sample:new(0)
  all={b4}
  copy={}
  copy[0]=b4
  for _,r in pairs(data.rows) do all[1]:add(Y(r)) end
  math.randomseed(1)
  for _,k in pairs{15,20,25,30,40,80,160} do
    push(all,Sample:new(k))
    copy[k]=all[#all]
    for _=1,repeats do
      shuffle(data.rows)
      all[#all]:add(sort(map(data:around(k),Y))[1]) end end
  ---------
  first = sort(Sample.merges(sort(all,lt"mu"),b4.sd*0.35),SORTER)[1]
  want = first.txt
  rand=Sample:new(-1)
  push(all,rand)
  copy[-1] = rand
  for _=1,repeats do
    shuffle(data.rows)
    u={}; for j,row in pairs(data.rows) do
              if j > want then break else push(u,Y(row)) end  end
    rand:add(sort(u)[1])  end
  ---------
  all= sort(Sample.merges(sort(all,lt"mu"),b4.sd*0.35),SORTER)
  report = {G0((b4.mu - first._meta.mu)/(b4.mu - b4.lo)), --1 = delta
            #data.rows, -- 2 = nrows
            data.xs,    -- 3 = xs
            data.ys,    -- 4 = ys
            G(b4.lo)}   -- 5 = lo
  for _,k in pairs{0, --6 =
                   -1,15,20,25,30,40,80,160} do
    push(report, fmt("%.2f%s",copy[k].mu, copy[k]._meta.rank)) end
  push(report,(file or the.data):gsub("^.*/",""))
  print(table.concat(report,", ")) end

go["--all"]= function(_)
  run({"--header","--csv","--data","--xs","--ys","--around"}, go, the.rseed) end
-------------------------------------------------------------------------------
--   __                                
--  (_   _|_   _.  ._  _|_         ._  
--  __)   |_  (_|  |    |_    |_|  |_) 
--                                 |   

math.randomseed(the.rseed)
if not pcall(debug.getlocal,4,1) then  -- if this code is in charge
  for k,v in pairs(arg) do
    if go[v] then go[v](arg[k+1]) end end end 

return {the=the, Data=Data, Sym=Sym, Num=Num}
