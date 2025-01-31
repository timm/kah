local the,go={},{}
go["-h"]= function(_) print([[

kopt.lua : check kmeans++ centroids for good results
(c) 2025 Tim Menzies <timm@ieee.org>, MIT license.

USAGE:
  lua jopt.lua [OPTIONS] [DEMO]

OPTIONS:
  -h       print help
  -b int   number of bins; e.g. ]]..the.bins..[[ 
  -f file  data file; e.g. ]]     ..the.file..[[  
  -r int   random seed; e.g. ]]   ..the.seed..[[   

DEMOS:
  --csv
  --data ]]) end

-- THe column names in row1 of the input csv files dindicates column meta data:
--  
-- - **`^[A-Z]`**: Columns that start with an uppercase letter are treated 
--   as **numeric columns** (`Num`).
-- - **`X$`**:  Columns that end with `X` are **ignored** (not added to 
--   `self.x` or `self.y`).
-- - **`!$`**: Columns that end with `!` are treated as the **class/target 
--   column** (`self.klass`).
-- - **`+$` or `-$`**: Columns that end with `+` or `-` are treated as 
--   **dependent/target variables** and added to `self.y`. **`+`** goals are 
--   to be maximized while
--   **`-`** goals are to be minimized.
-- - **Other names**:  Columns that do not match the above patterns are treated as
--   **independent non-numeric variables** and added to `self.x`.

the = {bins = 7, 
       seed = 1234567891,
       p    = 2,
       samples = 32,
       file = "../../moot/optimize/misc/auto93.csv"}

go["-b"] = function(s) the.bins = s + 0 end
go["-f"] = function(s) the.file = s     end
go["-r"] = function(s) the.seed = s + 0 end

-------- --------- --------- --------- --------- --------- --------- --------- -----------
local Big=1E32
local Row, Cols, Data, Sym, Num = {},{},{},{},{}

-------- --------- --------- --------- --------- --------- --------- --------- -----------
local abs, cos, log, sqrt = math.abs, math.cos, math.log, math.sqrt
local any, csv, fmt, _id, id, keysort, many, map, minkoski
local new, normal, o, oo, push, puts, sort, sum

function any(t) return t[math.random(#t)] end

function csv(file,      CELLS,src)
  CELLS=function(s,   z)
          for s1 in s:gmatch"([^,]+)" do z[1+#z]=s1:match"^%s*(.-)%s*$" end; return z end
  src = io.input(file)
  return function(      s)
    s = io.read()
    if s then return CELLS(s,{}) else io.close(src) end end  end

fmt = string.format

_id = 0
function id() _id = _id+1; return _id end 

function keysort(a, DO)
  local DECORATE   = function(x) return {DO(x),x} end
  local UNDECORATE = function(x) return x[2] end
  return map(sort(map(a, DECORATE), lt(1)), UNDECORATE) end

function lt(k) return function(a,b) return a[k] < b[k] end end

function map(t,DO,     u)
  u={}; for _,x in pairs(t) do u[1+#u] = DO(x) end; return u end

function minkowski(cols, p, FUN,  DO)
  DO = function(col) return FUN(col) ^ p end
  return (sum(cols, DO) / #cols) ^ (1/p) end

local function many(a,n,   z)
  z={}; for i = 1,(n or #a) do z[i]=any(a) end; return z end

local function new(meta, t) 
  meta.__index = meta
  meta.__tostring = meta.__tostring or o
  return setmetatable(t,meta) end

local function normal(mu,sd)
  return (mu or 0) + (sd or 1) * sqrt(-2*log(math.random()))
                               * cos(2*math.pi*math.random()) end

local function o(it,          t,NUM,LIST,DICT) 
  NUM = function() return fmt(it//1 == it and "%s" or "%.3g",it) end
  LIST= function() for k,v in pairs(it) do t[1+#t]=o(v) end end
  DICT= function() for k,v in pairs(it) do t[1+#t]=fmt(":%s %s",k,o(v)) end end
  t   = {}
  if type(it) == "number" then return NUM() end 
  if type(it) ~= "table"  then return tostring(it) end
  if #it>0 then LIST() else DICT(); table.sort(t) end
  return "{" .. table.concat(t, " ") .. "}" end

function oo(x) print(o(x)); return x end

function puts(t, col)
  for _,x in pairs(t) do
    col = col or (type(x) == "number" and Num:new() or Sym:new())
    col:put(x) end
  return col end

function push(t,x)  t[1+#t] = x; return x end

function sort(t,f)  table.sort(t,f); return t end

function sum(a,DO,     n)
  n=0; for _,x in pairs(a) do n = n + DO(x) end ; return n end

-------- --------- --------- --------- --------- --------- --------- --------- -----------
function Row:new(t) return new(Row,{cells=t, id=id()}) end 

function Data:new(src) return new(Data,{rows={}, cols=nil}):puts(src) end 

function Cols:new(t) 
  t = t.cells
  return new(Cols,{names=t,klass=nil,all={},x={},y={}}):inits(t) end

function Sym:new(s,n) return new(Sym,{txt=s,pos=n or 0,n=0,has={},mode=nil,most=0 }) end 

function Num:new(s,n) 
  return new(Num,{txt=s, pos=n or 0, n=0, lo=Big, hi= -Big, mu=0, m2=0, sd=0,
                  goal=(s or ""):find"-$" and 0 or 1}) end 

function Data:clone(src) return Data:new({Row:new(i.cols.names)}):puts(src) end

function Cols:inits(names,    col)
  for n,s in pairs(names) do
    col = (s:find"^[A-Z]" and Num or Sym):new(s,n) 
    push(self.all, col)                                 
    if not s:find"X$" then                             
      push(s:find"[!+-]$" and self.y or self.x,  col) 
      if s:find"!$" then self.klass=col end end end 
  return self end

-------- --------- --------- --------- --------- --------- --------- --------- -----------
function Row:get(col)
  return self.cells[col.pos] end

function Data:puts(src) 
  if   type(src)=="string"
  then for   row in csv(src)         do self:put(Row:new(row)) end
  else for _,row in pairs(src or {}) do self:put(row) end end
  return self end

function Data:put(row)
  if   self.cols 
  then self.cols:put(row) 
       push(self.rows, row)
  else self.cols = Cols:new(row) end end

function Cols:put(row,   PUT)
  PUT = function(col) return col:put(row:get(col)) end
  return map(self.all, PUT) end

function Sym:put(x)
  if x=="?" then return x end
  self.n = self.n + 1
  self.has[x] = 1 + (self.has[x] or 0)
  if self.has[x] > self.most then
    self.most, self.mode = self.has[x], x end  end

function Num:put(n,    delta)
  if n=="?" then return n end
  self.n  = self.n + 1
  n       = n + 0 
  delta   = n - self.mu
  self.mu = self.mu + delta/self.n
  self.m2 = self.m2 + delta*(n - self.mu)
  self.sd = self.n < 2 and 0 or (self.m2/(self.n - 1))^0.5
  self.lo = math.min(n, self.lo)
  self.hi = math.max(n, self.hi) end

-------- --------- --------- --------- --------- --------- --------- --------- -----------
function Data:sorted()
 self.rows = keysort(self.rows, function(row) return self:ydist(row) end) 
  return self.rows end

function Data:ydist(row,     DO)
  DO = function(c) return abs(c.goal - c:norm(row:get(c))) end
  return minkowski(self.cols.y, the.p, DO) end

function Data:xdist(row1,row2,    DO)
  DO = function(c) return c:dist(row1:get(c), row2:get(c)) end
  return minkowski(self.cols.x, the.p, DO) end

function Num:dist(p,q)
  if (p=="?" and q=="?") then return 1 end 
  p,q = self:norm(p), self:norm(q)
  p = p ~= "?" and p or (q<0.5 and 1 or 0) 
  q = q ~= "?" and q or (p<0.5 and 1 or 0) 
  return abs(p - q) end

function Sym:dist(p,q)
  if p=="?" and q=="?" then return 1 end 
  return p==q and 0 or 1 end

  function Data:neighbors(row1,rows)
    return keysort(rows or i.rows, function(row2) 
                                    return self:xdist(row1,row2) end ) end

function Data:centroids(k,  rows,      out)--> rows
  rows = rows or self.rows
  out = {any(rows)}
  for _ = 2,k do 
    local all,u = 0,{}
    for _ = 1, the.samples do
      local row = any(rows)
      local closest = self:neighbors(row, out)[2]
      all = all + push(u, {row=row, d=self:xdist(row,closest)^2}).d end 
    local i,r = 1,all * math.random()
    for j,x in pairs(u) do
      r = r - x.d
      if r <= 0 then i=j; break end end 
    push(out, u[i].row)
  end
  return out end

-------- --------- --------- --------- --------- --------- --------- --------- -----------
function Num:norm(x)
  return x=="?" and x or (x - self.lo) / (self.hi - self.lo) end

function Sym:ent(    DO)
  DO = function(n) return n/self.n * log(n/self.n,2) end
  return - sum(self.has, DO) end

-------- --------- --------- --------- --------- --------- --------- --------- -----------
go["--oo"] = function(_) 
   for _=1,99 do Row:new{} end
   oo(the) 
   oo(Row:new{1,2,3})  end

go["--csv"] = function(_,  n) 
  n = -1
  for row in csv(the.file) do 
    if n % 50 == 0 then print(n, o(row)) end 
    n = n + 1 end end

go["--sym"] = function(_, s)
  s = puts{"a","a","a","a","b","b","c"}
  assert(abs(s:ent() - 1.38) < 0.01)  end

go["--num"] = function(_, n)
  n = Num:new()
  for _ = 1,100 do n:put(normal(10,1)) end
  assert(abs(n.mu - 10) < 0.2 and abs(n.sd - 1) < 0.2) end

local function _cols(cols)
  for key,cols in pairs{x=cols.x, y=cols.y} do
    print(key)
    for _,col in pairs(cols) do print("  " ..  o(col)) end end end

go["--header"] = function(_,   t)
  _cols(Cols:new(Row:new{"Age","name","Happy-","sadX"})) end

go["--data"] = function(_,   d)
  _cols(Data:new(the.file).cols) end

go["--xdist"] = function(_,  d) 
  d= Data:new(the.file)
  one = d.rows[1]
  print("","   ",o(one.cells))
  for n,row in pairs(keysort(d.rows, function(a) return d:xdist(one,a) end)) do
     if n < 10 or n > #d.rows - 10 then 
       print(n, fmt("%.3f",d:xdist(row,one)), o(row.cells)) end end end

go["--ydist"] = function(_,  d) 
  d= Data:new(the.file)
  for n,row in pairs(Data:new(the.file):sorted()) do 
     if n < 10 or n > #d.rows - 10 then 
       print(n, fmt("%.3f",d:ydist(row)), o(row.cells)) end end end

go["--centroids"] = function(_,  d)
  d= Data:new(the.file)
  for _,row in pairs(d:centroids(24).rows) do oo(row.cells) end end

-------- --------- --------- --------- --------- --------- --------- --------- -----------
if not pcall(debug.getlocal,4,1) then  -- if this code is in charge
  for k,v in pairs(arg) do
    math.randomseed(the.seed)
    if go[v] then go[v](arg[k+1]) end end end 

return {the=the, Data=Data, Sym=Sym, Num=Num}
