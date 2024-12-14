local l={}

-- Lists
function l.push(t,x) t[1+#t] =x; return x end

function l.any(t) return t[math.random(#t)] end

-- Sort
function l.two(F) return function(a,b) return F(a) < F(b) end end

function l.sort(t,F) table.sort(t,F); return t end

function l.shuffle(t,    j) 
  for i = #t, 2, -1 do j = math.random(i); t[i], t[j] = t[j], t[i] end
  return t end

-- Select the items for `t` with least `F(x)` score.
function l.min(t,F,      n,lo,out)
  lo = math.huge
  for _,x in pairs(t) do
    n = F(x)
    if n < lo then lo,out = n,x end end
  return out end

-- Select return a key  form `t`, biased by the numeric values of those keys.
function l.pick(t,     u,r,all,anything)
  u,all=0
  for x,n in pairs(t) do l.push(u,{n,x}); all= all + n end
  r = math.random()
  for _,xn in pairs(l.sort(u,gt(1))) do
    r = r - xn[1]/all
    if r <= 0 then return xn[2] else anything = anything or xn[2] end end
  return anything end

-- String to things
function l.csv(file,     CELLS, src)
  CELLS = function(s,t)
            for s1 in s:gmatch"([^,]+)" do t[1+#t]=s1:match"^%s*(.-)%s*$" end
            return t end
  src = io.input(file)
  return function(s)
    s = io.read()
    if s then return CELLS(s,{}) else io.close(src) end end end


-- Thing to string
function l.o(it,          t,NUM,LIST,DICT) 
  NUM = function() return string.format(it//1 == it and "%s" or "%.3g",it) end
  LIST= function() for k,v in pairs(it) do t[1+#t]=l.o(v) end end
  DICT= function() for k,v in pairs(it) do t[1+#t]=string.format(":%s %s",k,l.o(v)) end end
  t   = {}
  if type(it) == "number" then return NUM() end 
  if type(it) ~= "table"  then return tostring(it) end
  if #it>0 then LIST() else DICT(); table.sort(t) end
  return "{" .. table.concat(t, " ") .. "}" end

-- Polymporhism
function l.new(meta, t) 
  meta.__index = meta
  meta.__tostring = meta.__tostring or l.o
  return setmetatable(t,meta) end

-- Runtime
function l.run(some, all, seed,       ok,msg,fails)
  fails = 0
  for _,one in pairs(some) do
    math.randomseed(seed or 1234567891)
    print(l.yellow(one))
    ok,msg = xpcall(all[one], debug.traceback)
    if   ok == false 
    then print(l.red("fail in "..one.." :"..msg)); fails=fails + 1
    else print(l.green("pass")) end 
  end 
  print(l.yellow(string.format("%s failure(s)",fails)))
  os.exit(fails) end

function l.yellow(s) return "\27[33m" .. s .. "\27[0m" end
function l.green(s)  return "\27[32m" .. s .. "\27[0m" end
function l.red(s)    return "\27[31m" .. s .. "\27[0m" end

--  Return
return l
