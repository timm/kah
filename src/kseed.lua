#!/usr/bin/env lua
local the,go={},{}
function go.h(_) print(string.format([[

kseed.lua : multi-objective optimization via kmeans++ initialization.
(c) 2024 Tim Menzies <timm@ieee.org>, MIT license.
   
USAGE:
  lua kseed.lua [OPTIONS] [DEMO]
   
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
  -around [file]   optimzation via kmeans++ initialization

]], the.data, the.p, the.rseed, the.samples)) end

-- Config options
the= {p= 2,
      data= "../../moot/optimize/misc/auto93.csv",
      rseed= 1234567891,
      samples= 32}

local Big=1E32

-- Handlers for command line arguements -d, -p, -s, -r
function go.p(x) the.p = x+0 end 
function go.d(x) the.data = data end
function go.s(x) the.samples = x+0 end
function go.r(x) the.rseed = x+0; math.randomseed(the.rseed); end

-- Imports from standard librarieslibrary
local l=require"lib"
local any,  sort,  two,  shuffle,  norm,  push,  csv,  min,  pick,  o,  new =
    l.any,l.sort,l.two,l.shuffle,l.norm,l.push,l.csv,l.min,l.pick,l.o,l.new

-------------------------------------------------------------------------------
-- ### Structs

-- This code is so simmple, it only needs summaries of numeric columns
local Num, Data = {},{}

-- Summarize numeric columns
function Num:new(name) 
  return new(Num,{lo= Big,    -- smallest number seen in a column 
                  hi= -Big,   -- largest number seen in a column
                  utopia = name:find"-$" and 0 or 1 -- (min|max)imize = 0,1
                 }) end

-- Holds the rows and column sumamres.
function Data:new(names) 
  self = new(Data, {
      num ={}, -- num[i] : list[Num]
      x=   {}, -- independent columns (keyed by column number)
      y=   {}, -- dependent columns (keyed by column number)
      rows={}  -- set of rows
      })
  for k,s in pairs(names) do
    if s:find"^[A-Z]" then self.num[k] = Num:new(s) end
    if not s:find"X$" then
      if s:find"[!+-]$" then self.y[k] = self.num[k] else self.x[k] = k end end end
  return self end
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
function make(src, data)
  for t in csv(src)  do 
    if data then data:add(t) else data=Data:new(t)  end end
  return data end

-- Normalize a number 0..1
function Num:norm(x)
  return x=="?" and x or (x - self.lo) / (self.hi - self.lo) end
-------------------------------------------------------------------------------
-- ## Distance

-- IB1's distance between independent variables in row `t1`, `t2`. See section 2.4 of 
-- https://link.springer.com/article/10.1007/BF00153759
function Data:xdist(t1,t2,  n,d,DIST)
  DIST= function(num, a,b) 
          if (a=="?" and b=="?") then return 1 end -- if all unknown, assume max
          if not num then return a == b and 0 or 1 end -- non-numerics are easy
          a,b = num:norm(a), num:norm(b) 
          a = a ~= "?" and a or (b<0.5 and 1 or 0) -- when isnf doubt, assume max
          b = b ~= "?" and b or (a<0.5 and 1 or 0) -- when in dout, assume max
          return math.abs(a - b) end
  d,n=0,0
  for k,col in pairs(self.x) do
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
      r2 = min(out, function(ru) return self:xdist(r1,ru) end) -- who ru closest 2?
      u[r1]= self:xdist(r1,r2)^2 -- how close are you
    end
    push(out, pick(u)) -- stochastically pick one item 
  end 
  return out end
-------------------------------------------------------------------------------
-- ## Test cases

-- e.g. comamnd line option `lua kseed.lua -header` calls test case `eg.header(_)`.
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
-- ## Start-up 

math.randomseed(the.rseed)
if arg[0]:find"kseed" then
  for k,v in pairs(arg) do
    if go[v:sub(2)] then go[v:sub(2)](arg[k+1]) end end  end
	
return {the=the, Data=Data, Num=Num}
