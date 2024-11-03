
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

function NUM:delta(other)
  return abs(self.mu - other.mu) / ((1E-32 + self.sd^2/self.n + other.sd^2/other.n)^.5) end

function NUM:pooledSd(other)
  return sqrt(((self.n-1)*self.sd^2 + (other.n-1)*other.sd^2)/(self.n+other.n-2)) end 

function l.cliffs(xs,ys)
  local lt,gt,n = 0,0,0
  for _,x in pairs(xs) do
     for _,y in pairs(ys) do
       n = n + 1
       if y > x then gt = gt + 1 end
       if y < x then lt = lt + 1 end end end
  return abs(gt - lt)/n <= the.delta end -- 0.195 

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
