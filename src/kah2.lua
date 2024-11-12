local the = {
  guess ={ acquire= "exploit",
           enough = 50000000,
           start = 4,
           Stop  = 30},
  bayes  = {k     = 1, 
            m     = 2},
  stats     = {bootstraps=256,
            delta = 0.197,
            conf = 0.05},
  p     = 2,
  rseed = 1234567891, 
  train = "../../moot/optimize/misc/auto93.csv",
  Test  = 0.33}

---------------------------------------------------------------------
local BIG = 1E32
local any,coerce,csv,kap,keysort,lt,many,map,normal
local o,pop,push,shuffle,sort,split,sum
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

function addCol(i,x,     d) --> (NUM|SYM, x:atom) --> nil
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
local ydist, xdist, dist, neighbors, norm

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

function norm(i,x) --> (COL, num) -> num
  return x=="?" and x or (x - i.lo)/(i.hi - i.lo + 1/BIG) end

---------------------------------------------------------------------
local like,loglikes,guessMostLiked

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

function guessBest(i) --> (Data) --> rows, XXX
  local acq,y,b,r,br,init,test,train,done,todo,best,rest
  acq  = {}
  acq.exploit = function(b,r) return b / r end
  acq.explore = function(b,r) return (b + r)/abs(b-r) end
  acq.adapt   = function(b,r,    w) 
                  w =  1 - #done/the.guess.Stop
                  return (b + r*w)/abs(b*w - r) end
  y    = function(row) return ydist(i,row) end
  b    = function(row) return exp(loglikes(best,row, #done, 2)) end 
  r    = function(row) return exp(loglikes(rest,row, #done, 2)) end 
  br   = function(row) return acq[the.guess.acquire](b(row), r(row)) end
  train, test = split(shuffle(i.rows), min(the.guess.enough, the.Test*#i.rows))
  done, todo  = split(train, the.guess.start) 
  while true do
    done = keysort(done, y) 
    if #done > the.guess.Stop or #todo < 5 then break end 
    best,rest = clone(i),clone(i)
    for j,row in pairs(done) do addData(j<=sqrt(#done) and best or rest, row) end
    todo = keysort(todo, br)             
    for _=1,2 do push(done, pop(todo,1)); push(done, pop(todo)) end end
  return done[1], keysort(test,br)[#test] end   

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
  return type(x)=="number" and fmt("%.2g",x) or  
         type(x)~="table"  and tostring(x) or 
         (x.is or "") .. "(" .. table.concat(f(x)," ") .. ")" end 

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

function Some(t,txt,    i) 
  i= {is="Some", txt=txt, rank-0, has={}, num=Num(), n=0, mu=0} 
  for _,x in pairs(t or {}) do addSome(i,x) end
  return i end

function addSome(i, x)
  addCol(i.num, x)
  push(i.has, x) 
  i.mu = i.num.mu
  i.n  = i.num.n end

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
function bootstrap(y0,z0,  bootstraps,conf)
  local x,y,z,yhat,zhat,n,this,that,b,obs
  local obs= function(i,j) return abs(j.mu - k.mu) / ((1E-32 + j.sd^2/j.n + k.sd^2/k.n)^.5) end
  local N= function(t,  n) n=n or Num(); for _,x in pairs(t) do addCol(n,x) end; return n end
  z,y,x  = N(z0), N(y0), N(y0, N(z0))
  yhat   = map(y0, function(y1) return y1 - y.mu + x.mu end)
  zhat   = map(z0, function(z1) return z1 - z.mu + x.mu end)
  n,b    = 0, (bootstraps or the.stats.wwBootstraps)
  for i=1, b do 
    if obs(N(many(yhat)), N(many(zhat ))) > obs(y,z) then n = n + 1 end end
  return n / b >= (conf or the.stats.conf) end

function scottKnot1(lo,hi,rank,somes, eps,rank)
  sum0,n0, sum1,n1 = 0,0,0,0
  for j=lo,li do
     some = somes[j]
     sum1 = sum1 + some.n*some.mu
     n1   = n1 + some.n end
  n= n1; mu = sum1/n1
  for j,num in pairs(some) do
     some = somes[j]
     sum0 = sum0 + some.n*some.mu; n1 = n0 + some.n; mu0 = sum0/n0 
     sum1 = sum1 - some.n*some.mu; n1 = n1 - some.n; mu1 = sum1/n1 
     now  = (n1 * (mu0 - mu)^2 + n2 * (mu1 - mu)^2 ) / n
     if now > most then
        diff,most,cut = now,j,mu1-mu0 end  end 
  if cut and diff > eps then
    ls,rs= {},{}
    for j=lo,hi do
      for _,x in pairs(somes[j].has) do push(j <=cut and ls or rs, x) end end
    if not (cliffs(ls,rs) and bootstraps(ls,rs)) then
      rank = scottKnot1(lo,cut,rank,somes,eps,rank) + 1
      rank = scottKnot1(cut+1,hi, rank,some,eps,rank)
      return rank end end
  for _,some in pairs(somes) do some.rank = rank end 
  return rank end

function scottKnot(some, eps)
  return scottKnot1(1,#somes,1,sort(somes,lt"mu"), eps) end


---------------------------------------------------------------------
local ok={}

function ok.acquire(s) the.guess.acquire=s end
function ok.Stop(s) the.guess.Stop=coerce(s) end
function ok.seed(s) the.seed=coerce(s); math.randomseed(the.seed)  end
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
      print(j,loglikes(d,row,1000,2)) end end end

function ok.guess(f,   d,asIs,toBe,rands,y,diff,cliffs)
  the.train = f
  cliffs=0.35
  d=read(the.train)
  asIs, toBe, after, rands=Num(), Num(),Num(),Num()
  y = function(row) return ydist(d,row) end
  diff=function(a,b) local x= abs(a.mu - b.mu)/(asIs.sd*cliffs); return x<1 and 0 or x end
  go2lo=function(a) local x= abs(a.mu - asIs.lo)/(asIs.sd*cliffs); return x<1 and 0 or 1 end
  map(d.rows, function(r) addCol(asIs, y(r)) end)
  for _=1,20 do 
     addCol(rands, (y(keysort(split(shuffle(d.rows),the.guess.Stop),y)[1])))
     train,test = guessBest(d)
     addCol(toBe,ydist(d,train))
     addCol(after,ydist(d,test))
     end
   print( o{x=#d.cols.x, b4=asIs.mu,b=the.guess.Stop,rand=rands.mu,diff=diff(toBe,rands),
            height=go2lo(toBe), lo=asIs.lo,guess=toBe.mu}, (f:gsub(".*/",""))) end
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
if arg[0] =="kah2.lua" then 
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
