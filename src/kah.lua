
#!/usr/bin/env lua
-- ok2.lua : using kmeans++ initializer, find one good example
-- (c) 2024 Tim Menzies <timm@ieee.org>, MIT license.

local DATA
local Big=1E32
local the= {p= 2,
            data= "../../moot/optimize/misc/auto93.csv",
            seed= 1234567891,
            samples= 32}

local Num, Data = {}, {}
local make

-------------------------------------------------------------------------------
local l=require"kahlib"
local any,  sort,  two,  shuffle,  norm,  push,  csv, min,    pick, o,  new =
    l.any,l.sort,l,two,l.shuffle,l.norm,l.push.l.csv,l.min,l.pick,l.o, l.new

------------------------------------------------------------------------------
local Num, Data = {},{}

function Num:new(name) 
  return new(Num,{lo= Big,    -- smallest number seen  
                  hi= -Big,   -- largest number seen
                  goal = name:find"-$" and 0 or 1 -- (min|max)imize = 0,1
                 }) end

function Data:new(names) 
  self = new(Data, {
      x=   {}, -- independent columns
      y=   {}, -- dependent columns
      num ={}, -- num[i] = {goal=0 or 1, lo=.., hi=...}
      rows={}  -- set of rows
      })
  for k,s in pairs(names) do
    if s:find"^[A-Z]" then self.num[k] = Num:new(s) end
    if not s:find"X$" then
      if s:find"[!+-]$" then self.y[k] = self.num[k] else self.x[k] = k end end end
  return self end

------------------------------------------------------------------------------
function Num:add(x)
  if x=="?" then return x end
  x = x + 0
  self.lo = math.min(x, self.lo)
  self.hi = math.max(x, self.hi) 
  return x end

function Data:add(t)
  for k,num in pairs(self.num) do t[k] = num:add(t[k]) end
  push(self.rows, t) 
  return self end

function make(src, data,      ADD)
  ADD = function(t) if data then data:add(t) else data=Data:new(t)  end end
  if   type(src)=="string" 
  then for   t in csv(src)   do ADD(t) end
  else for _,t in pairs(src) do ADD(t) end end 
  return data end

function Num:norm(x)
  return x=="?" and x or (x - self.lo) / (self.hi - self.lo) end

-------------------------------------------------------------------------------
function Data:xdist(t1,t2,  n,d,DIST)
  DIST= function(num, a,b) 
          if (a=="?" and b=="?") then return 1 end
          if not num then return a == b and 0 or 1 end 
          a,b = num:norm(a), num:norm(b) 
          a = a ~= "?" and a or (b<0.5 and 1 or 0)
          b = b ~= "?" and b or (a<0.5 and 1 or 0)
          return math.abs(a - b) end
  d,n=0,0
  for k,col in pairs(self.x) do
    n = n + 1
    d = d + math.abs(DIST(self.num[k], t1[k], t2[k])) ^ the.p end
  return (d/n) ^ (1/the.p) end

function Data:ydist(t,    n,d)
  d,n=0,0
  for k,num in pairs(self.y) do
    n = n + 1
    d = d + math.abs(num:norm(t[k]) - num.goal) ^ the.p end
  return (d/n) ^ (1/the.p) end

function Data:around(k,  rows,      t,out,r1,r2,u)
  rows = rows or self.rows
  out = {any(rows)}
  for _ = 2,k do
    u={}
    for _ = 1,math.min(the.samples, #rows) do
      r1 = any(rows)
      r2 = min(out, function(ru) return self:xdist(r1,ru) end) -- who ru closest 2?
      u[r1]= self:xdist(r1,r2)^2 -- how close are you
    end
    push(out, pick(u)) -- stochastically pick one item 
  end 
  return out end

-------------------------------------------------------------------------------
local go={}

function go.seed(x) 
  the.seed = x+0; math.randomseed(the.seed); print(math.random()) end

function go.header(_,      data)
  data = Data:new{"name","Age","Shoesize-"}
  print(o(data)) end

function go.csv(file,    data)
  for row in csv(file or the.data) do print(o(row)) end end

function go.data(file,   data)
  data= make(file or the.data) 
  print(#data.rows, o(data.y)) end

function go.x(file,    data,X)
  data= make(file or the.data) 
  X = function(row) return data:xdist(data.rows[1], row) end
  XX= function(a,b) return X(a) < X(b) end
  for k,row in pairs(sort(data.rows,XX)) do
    if k==1 or k% 30==0 then print(o(row), X(row)) end end end 

function go.y(file,    data,Y)
  data= make(file or the.data) 
  Y = function(row) return data:ydist(row) end
  YY= function(a,b) return Y(a) < Y(b) end
  for k,row in pairs(sort(data.rows,YY)) do
    if k==1 or k% 30==0 then print(o(row), Y(row)) end end end 

function go.around(file,     data)
  data= make(file or the.data) 
  Y = function(row) return data:ydist(row) end
  for _=1,20 do
    shuffle(data.rows) 
    print(Y(sort(data:around(20),two(Y))[1])) end end

-------------------------------------------------------------------------------
math.randomseed(the.seed)
if o(arg):find"kah.lua" then
  for k,v in pairs(arg) do
    if go[v:sub(3)] then go[v:sub(3)](arg[k+1]) end end  end
	
return {the=the, Data=Data, Num=Num}
