
function l.push(t,x) t[1+#t] = x; return x end
function l.split(t,n)
  local u,v = {},{}
  for j,x in pairs(t) do l.push(j <= n and u or v,x) end
  return u,v end

function l.any(t)  return t[R(#t)] end

function l.many(t,n,  u) 
  u={}; for i=1,(n or #t) do u[i] = l.any(t) end; return u end

function l.normal(mu,sd) 
   return (mu or 0)+(sd or 1)*sqrt(-2*log(R()))*cos(2*pi*R()) end

function l.lt(x) 
  return function(a,b) return a[x] < b[x] end end

function l.shuffle(t,    j)
  for i = #t, 2, -1 do j = R(i); t[i], t[j] = t[j], t[i] end
  return t end

function l.sort(t,fn) table.sort(t,fn); return t end

function l.adds(it,t)
  for _,x in pairs(t or {}) do it:add(x) end; return it end

function l.kap(t,fn,    u) --> list
  u={}; for k,v in pairs(t) do u[1+#u]=fn(k,v)  end; return u end

function l.map(t,fn,     u) --> list
  u={}; for _,v in pairs(t) do u[1+#u] = fn(v)  end; return u end

function l.sum(t,fn,     n)
  n=0; for _,x in pairs(t) do n=n+(fn and fn(x) or x) end; return n end

function l.keysort(t,fn,     DECORATE,UNDECORATE)
  DECORATE   = function(x) return {fn(x),x} end
  UNDECORATE = function(x) return x[2] end
  return l.map(l.sort(l.map(t,DECORATE),l.lt(1)), UNDECORATE) end

function l.coerce(s,     F,TRIM) 
  TRIM= function(s) return s:match"^%s*(.-)%s*$" end
  F   = function(s) return s=="true" and true or s ~= "false" and s end
  return math.tointeger(s) or tonumber(s) or F(TRIM(s)) end

function l.csv(file,     src)
  if file and file ~="-" then src=io.input(file) end
  return function(     s,t)
    s = io.read()
    if   not s 
    then if src then io.close(src) end 
    else t={}; for s1 in s:gmatch"([^,]+)" do t[1+#t]=l.coerce(s1)end; return t end end end

function l.datas(t,      i)
  i=0
  return function()
    while i<#t do
      i = i+1
      if t[i]:find"csv" then
        return t[i], DATA:new():read(t[i]) end end end end

function l.cli(t)
  for k,v in pairs(t) do
    v = tostring(v)
    for n,x in ipairs(arg) do
      if x=="-"..(k:sub(1,1)) then
        v= v=="false" and "true" or v=="true" and "false" or arg[n+1] end end 
    t[k] = l.coerce(v) end 
  return t end
l.fmt = string.format

function l.o(x,     F,G,GO) --> str
  F  = function() return #x>0 and l.map(x,l.o) or l.sort(l.kap(x,G)) end
  G  = function(k,v) if GO(k) then return l.fmt(":%s %s",k,l.o(x[k])) end end
  GO = function(k,v) return not l.o(k):find"^_" end
  return type(x)=="number" and l.fmt("%g",x) or  
         type(x)~="table"  and tostring(x) or 
         "{" .. table.concat(F()," ") .. "}" end 

function l.oo(x) print(l.o(x)) end

function l.new(klass, obj)
  klass.__index    = klass
  klass.__tostring = klass.__tostring or l.o
  return setmetatable(obj, klass) end
