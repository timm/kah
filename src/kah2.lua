local the = {
  acquire = 500,
  k     = 1, 
  m     = 2,
  p     = 2,
  rseed = 1234567891, 
  start = 4,
  Stop  = 30,
  train = "../../data/auto93.csv",
  Test  = 0.33}

---------------------------------------------------------------------
local BIG = 1E32
local coerce,csv,kap,keysort,lt,map,normal
local pop,push,o,shuffle,sort,split,sum
local abs, cos,exp,log = math.abs, math.cos, math.exp, math.log
local max,min,pi,R,sqrt = math.max, math.min, math.pi, math.random, math.sqrt

-------------------------------------------------------------------
local Num,Sym,Data,Cols

function Num(name,at) 
  return {is="Num", name=name, at=at, n=0, sd=0, mu=0, m2=0, 
          lo=BIG, hi=-BIG, goal = (name or ""):find"-$" and 0 or 1} end

function Sym(name,at) 
  return {is="Sym", name=name, at=at, n=0, has={}, mode=0, most=0} end

function Data(names)
  return {is="Data",rows={}, cols=Cols(names)} end

function Cols(names,    i,this)
  i = {is="Cols", names=names, all={}, x={}, y={}, klass=nil}
  for at,name in pairs(names) do
    this = push(i.all, (name:find"^[A-Z]" and Num or Sym)(name,at))
    if not name:find"X$" then
      if name:find"!$" then i.klass = this end
      push(name:find"[!+-]$" and i.y or i.x, this) end end
  return i end

---------------------------------------------------------------------
local addCol,addData,read,clone

function addCol(i,x,     d) --(i:NUM|SYM, x:atom): nil
  if x=="?" then return end
  i.n = i.n + 1
  if i.is=="Sym" then
    i.has[x] = 1 + (i.has[x] or 0)
    if i.has[x] > i.most then i.most, i.mode=i.has[x], x end 
  else
    d    = x - i.mu
    i.mu = i.mu + d/i.n
    i.m2 = i.m2 + d*(x - i.mu) 
    i.sd = i.n < 2 and 0 or (i.m2 / (i.n - 1))^0.5
    i.hi = max(x, i.hi)
    i.lo = min(x, i.lo) end  end

function addData(i,row)
  for k,v in pairs(row) do addCol(i.cols.all[k], v) end
  push(i.rows, row) end

function read(file,   i)
  for row in csv(file) do
    if i then addData(i,row) else i=Data(row) end end 
  return i end

function clone(i, rows,   j) --> Data1,[row] --> Data2
  j = Data(i.cols.names)
  for _,row in pairs(rows or {}) do addData(j, row) end
  return j end
---------------------------------------------------------------------
function ydist(i,row,  d) --> Data, row --> num
  d = function(y) return (abs(norm(y,row[y.at]) - y.goal))^the.p end
  return (sum(i.cols.y,d) /#i.cols.y)^(1/the.p) end

function dist(i,a,b) --> Col, atom, atom --> float
  if a=="?" and b=="?" then return 1 end
  if i.is== "Sym" then return (a==b and 0 or 1) end
  a,b = norm(i,a), norm(i,b)
  a = a ~= "?" and a or (b<0.5 and 1 or 0)
  b = b ~= "?" and b or (a<0.5 and 1 or 0)
  return abs(a-b) end

function xdist(i,row1,row2,    d,n) --> [row] ---> num
  d = function(x) return dist(i,row1[x.at], row2[x.at])^the.p  end
  return (sum(i.cols.x, d) / #i.cols.x) ^ (1/the.p) end

function neighbors(i,row,  rows) --> DATA,row,[row]? --> [row]
  return keysort(rows or i.rows, function(r) return xdist(i,r,row) end) end

---------------------------------------------------------------------
local like,likes,likesMost

function like(i,x,prior,     v,tmp)
  if i.is=="Sym" then
    return ((i.has[x] or 0) + the.m*prior) / (i.n + the.m)  
  else
    v = i.sd^2 + 1/BIG
    tmp = exp(-1*(x - i.mu)^2/(2*v)) / (2*pi*v) ^ 0.5
    return max(0,min(1, tmp + 1/BIG)) end end

function loglikes(i,row, nall, nh,          prior,f,l)
  prior = (#i.rows + the.k) / (nall + the.k*nh)
  f     = function(x) return l( like(x, row[x.at], prior) ) end
  l     = function(n) return n>0 and log(n) or 0 end
  return l(prior) + sum(i.cols.x, f) end

function guessMostLiked(i,      acq,y,b,r,br,init,test,train,done,todo,best,rest)
  acq  = acq or function(b,r) return b - r end
  y    = function(row) return ydist(i,row) end
  b    = function(row) return loglikes(best,row, #done, 2) end 
  r    = function(row) return loglikes(rest,row, #done, 2) end 
  br   = function(row) return acq(b(row), r(row)) end
  train, test = split(shuffle(i.rows), min(1024, the.Test*#i.rows))
  done, todo  = split(train, the.start) 
  while true do
    done = keysort(done, y) 
    if #done > the.Stop or #todo < 5 then break end 
    best,rest = clone(i),clone(i)
    for j,row in pairs(done) do col(j<=sqrt(#done) and best or rest, row) end
    todo = keysort(todo, br)                
    for _=1,2 do push(done, pop(todo,1)); push(done, pop(todo)) end end
  return done, _acquires(test, b,r) end   

---------------------------------------------------------------------
function normal(mu,sd) 
  return (mu or 0) + (sd or 1) * sqrt(-2*log(R())) * cos(2*pi*R()) end

function push(t,x)  t[1+#t]=x; return x end
function pop(t,n)   return table.remove(t,n) end

function kap(t,f,   u) u={}; for k,v in pairs(t) do u[1+#u]=f(k,v)        end; return u end
function map(t,f,   u) u={}; for _,v in pairs(t) do u[1+#u]=f(  v)        end; return u end
function sum(t,f,   n) n=0;  for _,v in pairs(t) do n=n+(f and f(v) or v) end; return n end

function lt(x) return function(a,b) return a[x] < b[x] end end
function sort(t,fn) table.sort(t,fn); return t end

function keysort(t,fn,     decorate,undecorate)
  decorate   = function(x) return {fn(x),x} end
  undecorate = function(x) return x[2] end
  return map(sort(map(t,decorate),lt(1)), undecorate) end

function shuffle(t,    k)
  for j = #t,2,-1 do k=R(j); t[j],t[k] = t[k],t[j] end; return t end

function split(t, n)
  u,v={},{}; for j,x in pairs(t) do push(j<=n and u or v,x) end; return u,v end

function o(x,     f,g,fmt) 
  fmt= string.format
  f= function(x) return #x>0 and map(x,o) or sort(kap(x,g)) end
  g= function(k,v) if k ~= "is" then return fmt(":%s %s",k,o(x[k])) end end
  return type(x)=="number" and fmt("%g",x) or  
         type(x)~="table"  and tostring(x) or 
         (x.is or "") .. "(" .. table.concat(f(x)," ") .. ")" end 

function coerce(s,     other,trim) 
  trim  = function(s) return s:match"^%s*(.-)%s*$" end
  other = function(s) return s=="true" and true or s ~= "false" and s end
  return math.tointeger(s) or tonumber(s) or other(trim(s)) end

function csv(file,     src)
  if file and file ~="-" then src=io.input(file) end
  return function(     s,t)
    s = io.read()
    if s then
      t={}
      for s1 in s:gmatch"([^,]+)" do t[1+#t]=coerce(s1) end
      return t 
    else 
      if src then io.close(src) end end end end

---------------------------------------------------------------------
local ok={}

function ok.train(s) the.train=s end

function ok.o(_) print(o(the)) end

function ok.num(_, n) 
  n = Num(); for _=1,1000 do addCol(n,normal(20,2)) end; print(o(n)) end

function ok.sym(_, s) 
  s = Sym(); for _,x in pairs{"a","a","a","a","b","b","c"} do addCol(s,x) end
  print(o(s)) end

function ok.data(_, d)
  d = read(the.train) 
  for _,col in pairs(d.cols.all) do print(o(col)) end end

function ok.like(_, d)
  d = read(the.train) 
  for j,row in pairs(d.rows) do 
    if j==1 or j%15==0 then
      print(j,loglikeData(d,row,1000,2)) end end end

-- function ok.bc()
--   all, other = nil,nil
--   for row in csv(the.train) do
--     if not all then all = Data(row) else
--       kl = row[#row]
--       other[kl]  = other[kl] or cloneData(all)
--       colData(all, row)
--       colData(other[kl], row) end
--     if #all.rows > 5 then xxx end end end  end
     
---------------------------------------------------------------------
if arg[0] =="b1.lua" then 
  math.randomseed(the.rseed)
  local fails = 0
  for k,s in pairs(arg) do
    s = s:sub(3)
    if ok[s] and false==ok[s](arg[k+1]) then fails=fails+1 end end
  os.exit(fails) end 

  -- # first things first. code something that runs a function names on command line
  -- # code in the repl. sh is your repl
  -- # code in spirts. 10 lines, run new test(s)
  -- # N-1 globals is better than N
  -- # malloc befre assign.
  --
  -- addX(i:X)
  -- repair: find a place to move with hihest goal.
  --         rule learnng = repair, results sorted by #changes.. how to average over all riles?
