local Sample={}
function Sample:new(s) 
  return new(Sample,{txt=s, n=0, mu=0, m2=0, sd=0, lo=1E32,all={}}) end

local l=require"lib"
local map, many, new, normal, push = l.map, l.many, l.new, l.normal, l.push

-------------------------------------------------------------------------------
local function adds(t,     num)
  num=Sample:new()
  for _,x in pairs(t) do num:add(x) end 
  return num end

-------------------------------------------------------------------------------
-- `boot(list[num], list[num], ?int=512, ?int=0.05) --> bool`   
-- Are `y0,z0` insignificantly different?
-- Taken from non-parametric significance test From Introduction to Bootstrap,
-- Efron and Tibshirani, 1993, chapter 20. https://doi.org/10.1201/9780429246593
-- Checks how rare are  the observed differences between samples of this data.
-- If not rare, then these sets are the same.
local function boot(y0,z0,  straps,conf,     x,y,z,yhat,zhat,n,N)
  z,y,x = adds(z0), adds(y0), adds(y0, adds(z0))
  yhat  = map(y0, function(y1) return y1 - y.mu + x.mu end)
  zhat  = map(z0, function(z1) return z1 - z.mu + x.mu end)
  n     = 0 
  for _ = 1,(straps or 512)  do
    if adds(l.many(yhat)):delta(adds(l.many(zhat))) > y:delta(z) 
    then n=n+1 end end
  return n / (straps or 512) >= (conf or 0.05)  end

-- From table1 of
-- https://journals.sagepub.com/doi/pdf/10.3102/10769986025002101
local function cliffs(xs,ys,  delta,      lt,gt,n)
  lt,gt,n,delta = 0,0,0,delta or 0.197
  for _,x in pairs(xs) do
      for _,y in pairs(ys) do
        n = n + 1
        if y > x then gt = gt + 1 end
        if y < x then lt = lt + 1 end end end
  return math.abs(gt - lt)/n <= delta end -- 0.195 

-------------------------------------------------------------------------------
function Sample:add(x,    d)
  self.all[1+#self.all] = x
	self.lo = math.min(self.lo,x)
  self.n  = self.n + 1
  d       = x - self.mu
  self.mu = self.mu + d / self.n
  self.m2 = self.m2 + d * (x - self.mu)
  self.sd = self.n < 2 and 0 or (self.m2/(self.n - 1))^.5  end

function Sample.delta(i,j)
  return math.abs(i.mu - j.mu) / ((1E-32 + i.sd^2/i.n + j.sd^2/j.n)^.5) end

function Sample:cohen(other,  d,     sd,i,j)
  i,j = self,other
  sd = (((i.n-1) * i.sd^2 + (j.n-1) * j.sd^2) / (i.n+j.n-2))^0.5
  return math.abs(i.mu - j.mu) <= (d or 0.35) * sd end

function Sample:same(other,  delta,straps,conf,i,j)
  i,j = self,other
  return cliffs(i.all,j.all,delta) and boot(i.all,j.all,straps,conf) end

function Sample:__tostring()
  return string.format("Sample{%s %g %g %g}",self.txt,self.n,self.mu,self.sd) end

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
     sample._meta.rank  = string.format("%c",96 + two[2]) end
  return samples end

local function normal(mu,sd,    r)
  return (mu or 0) + (sd or 1) * math.sqrt(-2*math.log(math.random()))
                                 * math.cos(2*math.pi*math.random()) end

-------------------------------------------------------------------------------
local go={}

function go.eg1(_,s,r)
  r=5
	print("r",string.format("\tmu\t\tsd")) 
	while r < 50000 do
	  r=r*2
	  s=Sample:new()
	  for i=1,r do s:add(normal(100,20)) end
	  print(r,string.format("\t%g\t\t%g",s.mu, s.sd)) end end

function go.eg2(_,dot,t,u)
  dot = function(s) return s and "y" or "." end
  print("d\tclif\tboot\tsame\tcohen")
  for d =1,1.25,0.02 do
     t = Sample:new(); for i=1,50 do t:add( normal(5,1) + normal(10,2)^2) end
     u = Sample:new(); for _,x in pairs(t.all) do u:add( x*d) end
     print(string.format("%.3f\t%s\t%s\t%s\t%s",
                         d,dot(cliffs(t.all,u.all)),dot(boot(t.all,u.all)), 
                           dot(t:same(u)), dot(t:cohen(u)))) end  end

-------------------------------------------------------------------------------
-- ## Start-up 
if arg[0]:find"stats" then
  math.randomseed(1234567891)
  for k,v in pairs(arg) do
    if go[v:sub(2)] then go[v:sub(2)](arg[k+1]) end end  end

return Sample
