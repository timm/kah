local l={}

function l.any(t) return t[math.random(#t)] end

function l.two(F) return function(a,b) return F(a) < F(b) end end

function l.sort(t,F) table.sort(t,F); return t end

function l.shuffle(t,    j) --> list
  for i = #t, 2, -1 do j = math.random(i); t[i], t[j] = t[j], t[i] end
  return t end

function l.push(t,x) t[1+#t] =x; return x end

function l.csv(file,     CELLS, src)
  CELLS = function(s,t)
            for s1 in s:gmatch"([^,]+)" do t[1+#t]=s1:match"^%s*(.-)%s*$" end
            return t end
  src = io.input(file)
  return function(s)
    s = io.read()
    if s then return CELLS(s,{}) else io.close(src) end end end

function l.min(t,F,      n,lo,out)
  lo = math.huge
  for _,x in pairs(t) do
    n = F(x)
    if n < lo then lo,out = n,x end end
  return out end

function l.pick(t,     all,anything,r)
  all=0
  for _,n in pairs(t) do all= all + n end
  r = math.random()
  for x,n in pairs(t) do
    anything = anything or x
    r = r - n/all
    if r <=0 then return x end end
  return anything end

function l.o(x,          t,FMT,NUM,LIST,DICT) 
  FMT = string.format
  NUM = function(x) return x//1 == x and "%s" or "%.3g" end
  LIST= function() for k,v in pairs(x) do t[1+#t] = o(v) end end
  DICT= function() for k,v in pairs(x) do t[1+#t] = FMT(":%s %s",k, o(v)) end end
  t   = {}
  if type(x) == "number" then return FMT(NUM(x),x) end 
  if type(x) ~= "table"  then return tostring(x) end
  if #x>0 then LIST() else DICT(); table.sort(t) end
  return "{" .. table.concat(t, " ") .. "}" end

function l.new(meta, t) 
  meta.__index = meta
  meta.__tostring = meta.__tostring or l.o
  return setmetatable(t,meta) end

return l
