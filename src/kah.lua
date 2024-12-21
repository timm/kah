local the,go={},{}
go["-h"]= function(_) print(string.format([[

kah.lua : peek at a few rows, find a near global best
(c) 2024 Tim Menzies <timm@ieee.org>, MIT license.

USAGE:
  lua kah.lua [OPTIONS] [DEMO]

OPTIONS:
  -d file  csv file of data (default: %s)
  -p int   coefficient of distance (default: %s)
  -r int   random number seed  (default: %s)
  -s int   #samples searched for each new centroid (default: %s)

DEMO:
  -header          header generation
  -csv    [file]   csv reader
  -data   [file]   loading rows
  -x      [file]   sorting rows by x values
  -y      [file]   sorting rows by y values
  -around [file]   find very good examples via kmeans++ initialization

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

-- ## Lib
-- ### Lists
local function push(a,x)--> x,  added to end of `a`.
  a[1+#a] =x; return x end

local function any(a)--> x (any items of `a`)
  return a[math.random(#a)] end

local function many(a,n,   z)--> a (`n` random items from `a`).
  z={}; for i = 1,(n or #a) do z[i]=any(a) end; return z end

local function items(a,    n)--> f.  Iterator for arrays.
  n=0
  return function() n=n+1; if n <= #a then return a[i] end end end

-- ### Sort
local function two(f)--> f, sorted by `f(x) < f(y)`.
  return function(p,q) return f(p) < f(q) end end

local function lt(x)--> f, sorted by  `p[s] < q[s]`.  
  return function(p,q) return p[s] < q[s] end end

local function gt(s)--> f,  sorted by  `a1[s] > a2[s]`.  
  return function(a1,a2) return a1[s] > a2[s] end end

local function sort(a,f)--> a, sorted via `f`. 
  table.sort(a,f); return a end

local function shuffle(a,    j)--> a, randomly re-ordered via Fisher-Yates [1]. 
  for i = #a, 2, -1 do j = math.random(i); a[i], a[j] = a[j], a[i] end
  return a end

-- ### Map
local function map(a,f,     z)--> a. Return items in `a` filtered through `f`.
  z={}; for _,x in pairs(a) do z[1+#z]=f(x) end ; return z end

local function l.min(a,f,      n,lo,z)--> x (ab item in `a` that scores least on `f`).
  lo = math.huge
  for _,x in pairs(a) do
    n = f(x)
    if n < lo then lo,z = n,x end end
  return z end

local function l.pick(d,     u,r,all,anything)--> x (a key in `d`) bias by `d`'s vals
  u,all={},0
  for x,n in pairs(d) do push(u,{n,x}); all= all + n end
  r = math.random()
  for _,xn in pairs(sort(u,gt(1))) do
    r = r - xn[1]/all
    if r <= 0 then return xn[2] else anything = anything or xn[2] end end
  return anything end

-- ## Strings to Things (and back again)
local fmt=string.format

local function yellow(s) return "\27[33m" .. s .. "\27[0m" end
local function green(s)  return "\27[32m" .. s .. "\27[0m" end
local function red(s)    return "\27[31m" .. s .. "\27[0m" end

local function l.csv(sFile,     f, src)--> f (iterator for csv rows)
  f = function(s2,z)
        for s3 in s2:gmatch"([^,]+)" do z[1+#z]=s3:match"^%s*(.-)%s*$" end
        return z end
  src = io.input(sFile)
  return function(      s1)
    s1 = io.read()
    if s then return f(s1,{}) else io.close(src) end end  end

local function o(x,          t,LIST,DICT)--> s. Generate a string for `x`.
  LIST= function() for k,v in pairs(x) do t[1+#t]= o(v) end end
  DICT= function() for k,v in pairs(x) do t[1+#t]= fmt(":%s %s",k,o(v)) end end
  t   = {}
  if type(x) == "number" then return fmt(x//1 == x and "%s" or "%.3g",x) end
  if type(x) ~= "table"  then return tostring(x) end
  if #x>0 then LIST() else DICT(); table.sort(t) end
  return "{" .. table.concat(t, " ") .. "}" end

-- ### Polymorphism
local function new(mt, a)--a, attached to a delegation table `mt`.
  mt.__index = meta
  mt.__tostring = mt.__tostring or o
  return setmetatable(a,mt) end

-------------------------------------------------------------------------------
-- ### Structs

local Num, Sym, Data = {},{}

function Sym:new(s,n)           -- Summarize numeric columns
  return new(Sym,{txt = s,      -- text about this column
                  pos = n or 0, -- column number
		  n = 0,        -- how many items?
		  has = {},     -- Symbol counts seen so far
		  mode = nil,   -- most common symbol
		  most = 0,     -- frequency of most common symbol
                 }) end

function Num:new(ns,n)        -- Summarize numeric columns
  return new(Num,{txt = s    -- text about this column
                  pos = n or 0 -- column number
		  n = 0,      -- how many items?
                  lo= Big,    -- smallest number seen in a column
                  hi= -Big,   -- largest number seen in a column
                  utopia = (s or ""):find"-$" and 0 or 1, -- (min|max)imize = 0,1
		  n = 0,      -- how many items?
		  mu= 0,      -- mean
		  m2= 0,      -- second moment (used for sd calcualtion)
		  sd= 0      -- standard deviaton
                 }) end

function Cols:new(ss)
  return new(Cols, {names = ss,
                    all   = {},
		    x     = {},
		    y     = {}})

function Data:new(src) -- Holds the rows and column summaries.
  self = new(Data, {
                  col = num ={}, -- num[i] : list[Num]
                  x=   {}, -- independent columns (keyed by column number)
                  y=   {}, -- dependent columns (keyed by column number)
      rows={},  -- set of rows
      xs=0,ys=0
      })
  for k,s in pairs(src()) do self:newHeader(k,s) end
  return self:adds(src) end

-- Create a new header. Store it anywhere that needs it,
function Data:newHeader(k,s)
  if s:find"^[A-Z]" then self.num[k] = Num:new(s) end
  if not s:find"X$" then
    if s:find"[!+-]$" then self.y[k] = self.num[k] else self.x[k] = k end
    if self.x[k] then self.xs=1+self.xs end
    if self.y[k] then self.ys=1+self.ys end end end
-------------------------------------------------------------------------------
-- ## Update

-- Update  knowledge of numeric columns. Ensures `x` is a numeric.
function Num:add(x)
  if x=="?" then return x end
  x = x + 0
  self.lo = math.min(x, self.lo)
  self.hi = math.max(x, self.hi)
  return x end

-- Updates the rows and column summaries.
function Data:add(t)
  for k,num in pairs(self.num) do t[k] = num:add(t[k]) end
  push(self.rows, t)
  return self end

-- Builds a new `Data` from a csv file.
function Data:adds(src)
  for t in src  do self:add(t) end
  return self end

-- Normalize a number 0..1
function Num:norm(x)
  return x=="?" and x or (x - self.lo) / (self.hi - self.lo) end
-------------------------------------------------------------------------------
-- ## Distance

-- IB1's distance between independent variables in row `t1`, `t2`. See S 2.4 of
-- https://link.springer.com/article/10.1007/BF00153759
function Data:xdist(t1,t2,  n,d,DIST)
  DIST= function(num, a,b)
          if (a=="?" and b=="?") then return 1 end -- if all unknown, assume max
          if not num then return a == b and 0 or 1 end -- non-numerics are easy
          a,b = num:norm(a), num:norm(b)
          a = a ~= "?" and a or (b<0.5 and 1 or 0) -- when in doubt, assume max
          b = b ~= "?" and b or (a<0.5 and 1 or 0) -- when in doubt, assume max
          return math.abs(a - b) end
  d,n=0,0
  for k,_ in pairs(self.x) do
    n = n + 1
    d = d + math.abs(DIST(self.num[k], t1[k], t2[k])) ^ the.p end
  return (d/n) ^ (1/the.p) end

-- Return distance to utopia of the dependent variables.
function Data:ydist(t,    n,d)
  d,n=0,0
  for k,num in pairs(self.y) do
    n = n + 1
    d = d + math.abs(num:norm(t[k]) - num.utopia) ^ the.p end
  return (d/n) ^ (1/the.p) end

-- kmeans++ initialization. New centroids are distance^2  from existing ones.
function Data:around(k,  rows,      t,out,r1,r2,u)
  rows = rows or self.rows
  out = {any(rows)}
  for _ = 2,k do
    u={}
    for _ = 1,math.min(the.samples, #rows) do
      r1 = any(rows)
      r2 = min(out,function(ru) return self:xdist(r1,ru) end) --who u closest 2?
      if r1 and r2 then
        u[r1]= self:xdist(r1,r2)^2 end-- how close are you
    end
    push(out, pick(u)) -- stochastically pick one item
  end
  return out end
-------------------------------------------------------------------------------
-- ## Test cases
local function run(ss, dfun, nSeed,       ok,msg,fails)
  fails = 0
  for _,one in pairs(ss) do
    math.randomseed(sSeed or 1234567891)
    ok,msg = xpcall(dfun[one], debug.traceback)
    if   ok == false 
    then print(l.red("FAILURE for '"..one.."' :"..msg)); fails=fails + 1
    else print(l.green("pass for '"..one.."'")) end 
  end 
  print(l.yellow(fmt("%s failure(s)",fails)))
  os.exit(fails) end


local Sample=require"stats"

-- e.g. command line option `lua kseed.lua -header` calls `eg.header(_)`.
function go.header(_,      data)
  data = Data:new(items({{"name","Age","Shoesize-"}}))
  print(o(data)) end

function go.csv(file,    data,k)
  k=0
  for row in csv(file or the.data) do
    k=k+1
    if k==1 or  k%30==0 then print(o(row)) end end end

function go.data(file,   data) data= Data:new(csv(file or the.data))
  print(#data.rows, o(data.y)) end

function go.xs(file,    data,X,XX)
  data= Data:new(csv(file or the.data))
  X = function(row) return data:xdist(data.rows[1], row) end
  XX= function(a,b) return X(a) < X(b) end
  for k,row in pairs(sort(data.rows,XX)) do
    if k==1 or k% 30==0 then print(o(row), X(row)) end end end

function go.ys(file,    data,Y,YY)
  data= Data:new(csv(file or the.data))
  Y = function(row) return data:ydist(row) end
  YY= function(a,b) return Y(a) < Y(b) end
  for k,row in pairs(sort(data.rows,YY)) do
    if k==1 or k% 30==0 then print(o(row), Y(row)) end end end

function go.around(file,     data,Y)
  data= Data:new(csv(file or the.data))
  Y = function(row) return data:ydist(row) end
  for _=1,20 do
    shuffle(data.rows)
    print(Y(sort(data:around(20),two(Y))[1])) end end

function go.compare(file,  SORTER,Y,X,G,G0,all,b4,copy,repeats,data,all,first,want,rand,u,report)
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
  run({"header","csv","data","xs","ys","around"}, go, the.seed) end
-------------------------------------------------------------------------------
-- ## Start-up
math.randomseed(the.rseed)
if arg[0]:find"kseed" then
  for k,v in pairs(arg) do
    if go[v] then go[v](arg[k+1]) end end  end

return {the=the, Data=Data, Sym=Sym, Num=Num}
