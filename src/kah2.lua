local the = {
  guess ={ acquire= "exploit",
           enough = 50000000,
           start  = 4,
           stop   = 30},
  bayes  = {k     = 1,
            m     = 2},
  stats = {bootstraps = 512,
            delta     = 0.197,
            conf      = 0.05,
            cohen     = 0.2},
  p     = 2,
  rseed = 1234567891, 
  train = "../../moot/optimize/misc/auto93.csv",
  Test  = 0.33}

---------------------------------------------------------------------
local BIG = 1E32
local any,coerce,csv,kap,keysort,lt,many,map,norm,normal
local o,pop,push,repeats,shuffle,sort,split,sum
local abs, cos,exp,log = math.abs, math.cos, math.exp, math.log
local max,min,pi,R,sqrt = math.max, math.min, math.pi, math.random, math.sqrt

-------------------------------------------------------------------
local Num,Sym,Data,Cols

function Num(name,at) --> (str, int) --> Num
  return {is="Num", name=name, at=at, n=0, sd=0, mu=0, m2=0, 
          lo=BIG, hi=-BIG, goal = (name or ""):find"-$" and 0 or 1} end

function Sym(name,at) --> (str, int) --> Sym
  return {is="Sym", name=name, at=at, n=0, has={}, mode=0, most=0} end

function Data(names) --> ( [str] ) --> Data
  return {is="Data",rows={}, cols=Cols(names)} end

function Cols(names,    i,this) --> ( [str] ) --> Cols
  i = {is="Cols", names=names, all={}, x={}, y={}, klass=nil}
  for at,name in pairs(names) do
    this = push(i.all, (name:find"^[A-Z]" and Num or Sym)(name,at))
    if not name:find"X$" then
      if name:find"!$" then i.klass = this end
      push(name:find"[!+-]$" and i.y or i.x, this) end end
  return i end

---------------------------------------------------------------------
local addCol,addData,read,clone

function addCol(i,x,     d) --> (NUM|SYM, atom) --> nil
  if x=="?" then return end
  i.n = i.n + 1
  if i.is=="Sym" then
    i.has[x] = 1 + (i.has[x] or 0)
    if i.has[x] > i.most then i.most, i.mode=i.has[x], x end 
  else -- todo: handle n>1 for Nums 
    d    = x - i.mu
    i.mu = i.mu + d/i.n
    i.m2 = i.m2 + d*(x - i.mu) 
    i.sd = i.n < 2 and 0 or (i.m2 / (i.n - 1))^0.5
    i.hi = max(x, i.hi)
    i.lo = min(x, i.lo) end  end

function addData(i,row) --> (DATA, row) --> nil
  for k,v in pairs(row) do addCol(i.cols.all[k], v) end
  push(i.rows, row) end

function read(file,   i) --> (str) --> Data
  for row in csv(file) do
    if i then addData(i,row) else i=Data(row) end end 
  return i end

function clone(i, rows,   j) --> (Data1, [row]) --> Data2
  j = Data(i.cols.names)
  for _,row in pairs(rows or {}) do addData(j, row) end
  return j end

---------------------------------------------------------------------
local norm,pooledSd

function norm(i,x) --> (COL, num) -> num
  return x=="?" and x or (x - i.lo)/(i.hi - i.lo + 1/BIG) end

function pooledSd(i,j)
  return sqrt(((i.n-1)*i.sd^2 + (j.n-1)*j.sd^2)/(i.n+j.n-2)) end 

---------------------------------------------------------------------
local ydist, xdist, dist, neighbors

function dist(i,a,b) --> (Col, atom, atom) --> num
  if a=="?" and b=="?" then return 1 end
  if i.is== "Sym" then return (a==b and 0 or 1) end
  a,b = norm(i,a), norm(i,b)
  a = a ~= "?" and a or (b<0.5 and 1 or 0)
  b = b ~= "?" and b or (a<0.5 and 1 or 0)
  return abs(a-b) end

function xdist(i,row1,row2,    d,n) --> (Data, row, row) ---> num
  d = function(x) return dist(i,row1[x.at], row2[x.at])^the.p  end
  return (sum(i.cols.x, d) / #i.cols.x) ^ (1/the.p) end

function ydist(i,row,  d) --> (Data, row) --> num
  d = function(y) return (abs(norm(y,row[y.at]) - y.goal))^the.p end
  return (sum(i.cols.y,d) /#i.cols.y)^(1/the.p) end

function neighbors(i,row,  rows) --> (DATA, row, [row]?) --> [row]
  return keysort(rows or i.rows, function(r) return xdist(i,r,row) end) end

---------------------------------------------------------------------
local like,loglikes,guessBest

function like(i,x,prior,    v,tmp) --> (Col,atom,num) --> num
  if i.is=="Sym" then
    return ((i.has[x] or 0) + the.bayes.m*prior) / (i.n + the.bayes.m)
  else
    v = i.sd^2 + 1/BIG
    tmp = exp(-1*(x - i.mu)^2/(2*v)) / (2*pi*v) ^ 0.5
    return max(0,min(1, tmp + 1/BIG)) end end

function loglikes(i,row, nall, nh,    prior,f,l) --> (Data,rows,int,int) --> num
  prior = (#i.rows + the.bayes.k) / (nall + the.bayes.k*nh)
  f     = function(x) return l( like(x, row[x.at], prior) ) end
  l     = function(n) return n>0 and log(n) or 0 end
  return l(prior) + sum(i.cols.x, f) end

function guessBest(i, sortp) --> (Data) --> rows, XXX
  local acq,y,b,r,br,init,test,train,done,todo,best,rest,stop
  stop = the.guess.stop
  acq= {
    exploit  = function(b,r) return b / r end,
    explore  = function(b,r) return (b + r)/(abs(b-r) + 1/BIG) end,
    adapt    = function(b,r) local w = (1 - #done/stop)  
                             return (b+r*w) / (abs(b*w - r) + 1/BIG) end }
  y          = function(row) return ydist(i,row) end
  b          = function(row) return exp(loglikes(best, row, #done, 2)) end 
  r          = function(row) return exp(loglikes(rest, row, #done, 2)) end 
  br         = function(row) return acq[the.guess.acquire](b(row), r(row)) end
  train,test = split(shuffle(i.rows), min(the.guess.enough, the.Test*#i.rows))
  done, todo = split(train, the.guess.start) 
  while true do
    done = keysort(done, y) 
    if #done > stop or #todo < 5 then break end 
    best,rest = clone(i),clone(i)
    for j,row in pairs(done) do addData(j<=sqrt(#done) and best or rest, row) end
    todo = keysort(todo, br)             
    for _=1,2 do push(done, pop(todo,1)); push(done, pop(todo)) end end
  return done, (sortp and keysort(test,br) or test) end   

---------------------------------------------------------------------
function normal(mu,sd) --> (num, num) --> 0..1
  return (mu or 0) + (sd or 1) * sqrt(-2*log(R())) * cos(2*pi*R()) end

function push(t,x) --> (list,any) --> any
    t[1+#t]=x; return x end
function pop(t,n)   return table.remove(t,n) end

function kap(t,f,   u) --> (list,func) --> t
  u={}; for k,v in pairs(t) do u[1+#u]=f(k,v) end; return u end

function map(t,f,   u) --> (list,func) --> t
  u={}; for _,v in pairs(t) do u[1+#u]=f(  v) end; return u end

function sum(t,f,   n) --> (list,func) --> t
  n=0;  for _,v in pairs(t) do n=n+f(v) end; return n end

function lt(x) --> (atom) --> func
  return function(a,b) return a[x] < b[x] end end

function sort(t,fn) --> (list,func) --> list
  table.sort(t,fn); return t end

function keysort(t,fn) --> (list,func) --> list
  local decorate   = function(x) return {fn(x),x} end
  local undecorate = function(x) return x[2] end
  return map(sort(map(t,decorate),lt(1)), undecorate) end

function shuffle(t,    k) --> (list) --> t
  for j = #t,2,-1 do k=R(j); t[j],t[k] = t[k],t[j] end; return t end

function any(t)  
  return t[R(#t)] end

function many(t,n,  u) 
  u={}; for i=1,(n or #t) do u[i] = any(t) end; return u end

function split(t, n,     u,v) --> (list)
  u,v={},{}; for j,x in pairs(t) do push(j<=n and u or v,x) end; return u,v end

function o(x,     f,g,fmt) --> (any) --> str
  fmt= string.format
  f= function(x) return #x>0 and map(x,o) or sort(kap(x,g)) end
  g= function(k,v) if k ~= "is" then return fmt(":%s %s",k,o(x[k])) end end
  return type(x)=="number" and fmt(x//1==x and "%s" or "%.2g",x) or  
         type(x)~="table"  and tostring(x) or 
         (x.is or "") .. "(" .. table.concat(f(x)," ") .. " )" end 

function coerce(s,     other,trim) --> string --> atom
  trim  = function(s) return s:match"^%s*(.-)%s*$" end
  other = function(s) return s=="true" and true or s ~= "false" and s end
  return math.tointeger(s) or tonumber(s) or other(trim(s)) end

function csv(file,     src) --> str --> func
  if file and file ~="-" then src=io.input(file) end
  return function(     s,t)
    s = io.read()
    if s then
      t={}
      for s1 in s:gmatch"([^,]+)" do t[1+#t]=coerce(s1) end
      return t 
    else 
      if src then io.close(src) end end end end

function noop(...) end
-------------------------------------------------------------------------------
local same, cliffs, bootstrap

function same(x,y)
  return cliffs(x,y) and bootstrap(x,y) end

function cliffs(xs,ys,  delta)
  local lt,gt,n = 0,0,0
  for _,x in pairs(xs) do
      for _,y in pairs(ys) do
        n = n + 1
        if y > x then gt = gt + 1 end
        if y < x then lt = lt + 1 end end end
  return abs(gt - lt)/n <= (delta or the.stats.delta) end -- 0.195 
      
-- Taken from non-parametric significance test From Introduction to Bootstrap,
-- Efron and Tibshirani, 1993, chapter 20. https://doi.org/10.1201/9780429246593
-- Checks how rare are  the observed differences between samples of this data.
-- If not rare, then these sets are the same.
function bootstrap(y0,z0,  bootstraps,conf)
  local x,y,z,yhat,zhat,n,b
  local obs= function(j,k) return abs(j.mu - k.mu) / ((1E-32 + j.sd^2/j.n + k.sd^2/k.n)^.5) end
  local N= function(t,  n) n=n or Num(); for _,x in pairs(t) do addCol(n,x) end; return n end
  z,y,x  = N(z0), N(y0), N(y0, N(z0))
  yhat   = map(y0, function(y1) return y1 - y.mu + x.mu end)
  zhat   = map(z0, function(z1) return z1 - z.mu + x.mu end)
  n,b    = 0, (bootstraps or the.stats.bootstraps)
  for i=1, b do 
    if obs(N(many(yhat)), N(many(zhat ))) > obs(y,z) then n = n + 1 end end
  return n / b >= (conf or the.stats.conf) end

-------------------------------------------------------------------------------
local Some, addSome, merge, merges

function Some(t,txt,    i) 
  i= {is="Some", txt=txt, rank=0, has={}, num=Num()} 
  for _,x in pairs(t or {}) do addSome(i,x) end
  return i end

function show(i) return string.format("%g%s",o(i.num.mu), i.rank==1 and "*" or " ") end

function addSome(i, x)
  if type(x)=="table" then 
    if x.is=="Some" then return addSome(i, x.has) end
    for _,y in pairs(x) do addSome(i, y) end
  else
    addCol(i.num, x)
    push(i.has, x) end end

function merge(i,j,     k) --> (Some,Some) --> Some
  k=Some(i.all, i.txt)
  for _,t in pairs{i.has,j.has} do
    for _,x in pairs(t) do
       addSome(k, x) end end
  return k end

function merges(somes,eps,   t) 
  somes = keysort(somes, function(m) return m.num.mu end)
  for j,some in pairs(somes) do
    if   t then
      if abs(some.num.mu - t[#t].num.mu) > eps and not same(some.has,t[#t].has) 
      then push(t,some) 
      else t[#t] = merge(some, t[#t]) end 
    else 
      t={some} 
    end
    some.rank = #t end
  return somes,t  end

---------------------------------------------------------------------
local ok={}


function ok.acquire(s) the.guess.acquire=s end
function ok.Stop(s) the.guess.stop=coerce(s) end
function ok.seed(s) the.seed=coerce(s); math.randomseed(the.rseed)  end
function ok.train(s) the.train=s end

function ok.o(_) print(o(the)) end

function ok.num(_, n) 
  n = Num(); for _=1,100 do addCol(n,normal(20,2)) end; print(o(n)) end

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
      print(j,loglikes(d,row,1000,2)) end end end

function ok.stats0(   r,t,u,d,y,n1,n2)
  local N= function(t,  n) n=n or Num(); for _,x in pairs(t) do addCol(n,x) end; return n end
  y = function(s) return s and "y" or "." end
  d,r= 1,100
  print("d\tclif\tboot\tcohen")
  while d< 1.2 do
    t={}; for i=1,r do t[1+#t] = normal(10,1) + normal(10,2)^2 end 
    u={}; for i,x in pairs(t) do  u[i] = x*d end
    d=d*1.01
    n1,n2 = N(t), N(u)
    print(string.format("%.3f\t%s\t%s\t%s", 
                        d, y(cliffs(t,u)), y(bootstrap(t,u)), 
                           y(abs(n1.mu - n2.mu) < .35*pooledSd(n1,n2)))) end end

function repeats(t,n,    u)
  u={}; for _ = 1,n do for _,x in pairs(t) do u[1+#u] = x end end; return u end 

local function _stats(somes)
  for _,some in pairs(merges(somes, 0.01)) do
    print(show(some), some.txt) end end

function ok.stats1(   n)
  n=5
  _stats{
         Some(repeats({0.34, 0.49 ,0.51, 0.6}, n), "x1"),
         Some(repeats({0.13 ,0.23, 0.38, 0.38},n), "x3"),
         Some(repeats({0.6  ,0.7,  0.8 , 0.9}, n), "x4"),
         } end

function ok.stats2(   n)
  n=5
  _stats{
         Some(repeats({0.34, 0.49 ,0.51, 0.6}, n), "x1"),
         Some(repeats({0.6  ,0.7 , 0.8 , 0.89},n), "x2"),
         Some(repeats({0.13 ,0.23, 0.38, 0.38},n), "x3"),
         Some(repeats({0.6  ,0.7,  0.8 , 0.9}, n), "x4"),
         Some(repeats({0.1  ,0.2,  0.3 , 0.4}, n), "x5")
         } end

function ok.stats3(   n)
  n=5
  _stats{
         Some(repeats({0.34, 0.49 ,0.51, 0.6}, n), "x1"),
         Some(repeats({0.34, 0.49 ,0.51, 0.6}, n), "x2"),
         Some(repeats({0.34, 0.49 ,0.51, 0.6}, n), "x3"),
         } end

function ok.stats4(   n)
  n=5
  _stats{
    Some({4,5,7,8,9,6,5,4,4,5,6,6,7,8,8,7,6,5,4,4,5,6,7,8,9,8,7,5,4},"oranges"),
    Some({6,7,8,9,0,9,8,7,6,7,8,9,0,9,8,7,6,7,8},"pears"),
    Some({1,12,13,15,14,13,11,12,13,14,15,14,13,11,12,13,14,15,
          17,18,10,19,10,19,17,16,15,16,15,17,17,19,10,10,18,17,
          16,15,15,16,17,17,19,19,17},"bananas"),
    Some({79,86,86,77,65,77,72,73,65,56,60,69,75,70,62,56,69,55,67,99},"DEcf.5f"),
    Some({27,27,26,2,25,26,10,20,31,6,17,25,25,19,25,23,16,5,2,23},"GQ.05p10c2"),
    Some({37,62,55,56,63,36,34,65,56,52,33,46,41,34,40,50,40,61,35,65,5},"GA.01p10c1")
  } end

function ok.guess(f,   d,asIs,toBe,after,rands,y,cliffs)
  the.train = f or the.train
  cliffs = 0.35
  d = read(the.train)
  rx  = {asIs=Some({},"asIs"), rand=Some({},"rand"), explore=Some({},"explore"), 
          exploit=Some({},"exploit"), adapt=Some({},"adapt")}
  y     = function(row) return ydist(d,row) end
  map(d.rows, function(r) addSome(rx.asIs, y(r)) end)
  for _=1,20 do 
     addSome(rx.rand, (y(keysort(split(shuffle(d.rows),the.guess.stop),y)[1])))
     for _,acq in pairs{"exploit","explore","adapt"} do
       the.guess.acquire = acq
       local train,test = guessBest(d,false)
       addSome(rx[acq], y(train[1])) end
  end
  merges(rx, rx.asIs.num.sd * the.stats.cohen)
  toBe = (rx.explore.num.mu + rx.exploit.num.mu + rx.adapt.num.mu ) /3
  print( o{delta   = (rx.asIs.num.mu - toBe)/rx.asIs.num.mu,
           x       = #d.cols.x, 
           y       = #d.cols.y, 
           rows    = #d.rows,
           b4      = rx.asIs.num.mu,
           toBe    =  toBe,
           lo      = rx.asIs.num.lo,
           close   = (toBe - rx.asIs.num.lo)/(rx.asIs.num.sd*cliffs) >= 1 and "y" or ".",
           asIs    = show(rx.asIs),
           rand    = show(rx.rand),
           explore = show(rx.explore),
           exploit = show(rx.exploit),
           adapt   = show(rx.adapt),
           file    = the.train:gsub(".*/",""),
           zstop   = the.guess.stop}) end
     
function ok.all(_)
  tests{"--o","--num","--sym", "--data","--like","--stats0",
        "--stats1", "--stats2","--stats3","--stats4", print } end

---------------------------------------------------------------------
function tests(todo,  say,     fails)
  fails = 0
  for k,s in pairs(todo) do
    math.randomseed(the.rseed)
    s = s:sub(3)
    if ok[s] then
      if false==ok[s](arg[k+1]) 
      then say(s,"FAIL",("-"):rep(20))
           fails=fails+1 
      else say(s,"PASS",("+"):rep(5)) end end end
  os.exit(fails) end

if arg[0] =="kah2.lua" then tests(arg,noop) end
 
  -- # first things first. code something that runs a function names on command line
  -- # code in the repl. sh is your repl
  -- # code in spirts. 10 lines, run new test(s)
  -- # N-1 globals is better than N
  -- # malloc befre assign.
  --
  -- addX(i:X)
  -- repair: find a place to move with hihest goal.
  --         rule learnng = repair, results sorted by #changes.. how to average over all riles?
