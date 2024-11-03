
local EG={}
function EG.tests() 
  return l.tests(EG, {"any","sum","split","sort","num","sym","csv","stats"}) end

function EG.any(  a)
  a = {10,20,30,40,50,60}
  l.oo(l.map({l.any(a), l.many(a,3),l.shuffle(a),l.keysort(a,function(x) return -x end)},
             l.o)) end

function EG.sum()
  assert(210 == l.sum{10,20,30,40,50,60}) end

function EG.split(    a,b,c)
  a={10,20,30,40,50,60}
  b,c = l.split(a,3)
  print(l.o(b), l.o(c)) end

function EG.sort(    t)
  t={1,2,3,4,5,6,7}
  t=l.sort(t, function(x,y) return  x > y end)
  l.oo{10,4,5}
  l.oo(t) end

function EG.num(    n) 
  n = NUM:new()
  for _ = 1,1000 do n:add( l.normal(10,2) ) end
  assert(10-n.mu < 0.1 and 2-n.sd < 0.03) end

function EG.sym(    s) 
  s = l.adds(SYM:new(), {"a","a","a","a","b","b","c"})
  print(s.mode, l.o(s.has)) end

function EG.csv(   d, n)
  print(the.train)
  n=0
  for row in l.csv(the.train) do n=n+1; if n==1 or n%90==0 then l.oo(row) end end end

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

function EG.data(   d)
  d = DATA:new():read(the.train) 
  assert(3184 == (l.sum(d.rows,function(r) return #r end))) end

function EG.like(   d,n)
  n,d = 0,DATA:new():read(the.train) 
  for _,row in pairs(l.keysort(d.rows, function(r) return d:loglike(r,#d.rows,1) end)) do 
    n=n+1 
    if n==1 or n % 30==0 then 
      print(l.fmt("%3s : %6.2f : %s",n,d:loglike(row,#d.rows,2), l.o(row))) end end  end

function EG.acquire(     d, train,test)
  print(the.train)
  d = DATA:new():read(the.train) 
  n = l.adds(NUM:new(), l.map(d.rows, function(r) return d:ydist(r) end))
  train,test = d:acquire() 
  print(n.mu,d:ydist(train), d:ydist(test)) end 

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
    
