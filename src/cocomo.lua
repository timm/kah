local Cocomo = {}
local int,from,within,o,kap,map,sort

function Cocomo.effort(w,y)
  local em, sf, m = 1, 0, Cocomo.meta()
  for k,w1 in pairs(w) do
    if     m[k][1] == "+" then em = em * w1
    elseif m[k][1] == "-" then em = em * w1 
    else                       sf = sf + w1 end end 
  return y.AX*y.loc^(y.BX + 0.01*sf) * em end

function Cocomo.mxplusb(w,x)
  if w=="+" then return (x-3)*from( 0.073,  0.21 ) + 1 end
  if w=="-" then return (x-3)*from(-0.187, -0.078) + 1 end
  if w=="*" then return (x-6)*from(-1.56,  -1.014) end end

function Cocomo.meta(    p,n,s)
  p,n,s = "+","-","*"
  return {
    Loc = {"1",2,200},
    Acap= {n}, Cplx={p,1,6}, Prec={s,1,6},
    Aexp= {n}, Data={p,2,5}, Flex={s,1,6},
    Ltex= {n}, Docu={p},     Arch={s,1,6},
    Pcap= {n}, Pvol={p,2,5}, Team={s,1,6},
    Pcon= {n}, Rely={p},     Pmat={s,1,6},
    Plex= {n}, Ruse={p,2,6},
    Sced= {n}, Stor={p,3,6},
    Site= {n}, Time={p,3,6},
    Tool= {n}} end

function Cocomo.risks(   _,ne,nw,nw4,sw,sw4,ne46,sw26,sw46)
  _ = 0
  ne={{_,_,_,1,2,_}, -- bad if lohi
    {_,_,_,_,1,_},
    {_,_,_,_,_,_},
    {_,_,_,_,_,_},
    {_,_,_,_,_,_},
    {_,_,_,_,_,_}}
  nw={{2,1,_,_,_,_}, -- bad if lolo
    {1,_,_,_,_,_},
    {_,_,_,_,_,_},
    {_,_,_,_,_,_},
    {_,_,_,_,_,_},
    {_,_,_,_,_,_}}
  nw4={{4,2,1,_,_,_}, -- very bad if  lolo
    {2,1,_,_,_,_},
    {1,_,_,_,_,_},
    {_,_,_,_,_,_},
    {_,_,_,_,_,_},
    {_,_,_,_,_,_}}
  sw={{_,_,_,_,_,_}, -- bad if  hilo
    {_,_,_,_,_,_},
    {_,_,_,_,_,_},
    {1,_,_,_,_,_},
    {2,1,_,_,_,_},
    {_,_,_,_,_,_}}
  sw4={{_,_,_,_,_,_}, -- very bad if  hilo
    {_,_,_,_,_,_},
    {1,_,_,_,_,_},
    {2,1,_,_,_,_},
    {4,2,1,_,_,_},
    {_,_,_,_,_,_}}
  -- bounded by 1..6
  ne46={{_,_,_,1,2,4}, -- very bad if lohi
    {_,_,_,_,1,2},
    {_,_,_,_,_,1},
    {_,_,_,_,_,_},
    {_,_,_,_,_,_},
    {_,_,_,_,_,_}}
  sw26={{_,_,_,_,_,_}, -- bad if hilo
    {_,_,_,_,_,_},
    {_,_,_,_,_,_},
    {_,_,_,_,_,_},
    {1,_,_,_,_,_},
    {2,1,_,_,_,_}}
  sw46={{_,_,_,_,_,_}, -- very bad if hilo
    {_,_,_,_,_,_},
    {_,_,_,_,_,_},
    {1,_,_,_,_,_},
    {2,1,_,_,_,_},
    {4,2,1,_,_,_}}
  
  return {
    Cplx= {Acap=sw46, Pcap=sw46, Tool=sw46}, --12
    Ltex= {Pcap=nw4},  -- 4
    Pmat= {Acap=nw,   Pcap=sw46}, -- 6
    Pvol= {Plex=sw},  --2
    Rely= {Acap=sw4,  Pcap=sw4,  Pmat=sw4}, -- 12
    Ruse= {Aexp=sw46, Ltex=sw46},  --8
    Sced= {Cplx=ne46, Time=ne46, Pcap=nw4, Aexp=nw4, Acap=nw4,
           Plex=nw4,  Ltex=nw, Pmat=nw, Rely=ne, Pvol=ne, Tool=nw}, -- 34
    Stor= {Acap=sw46, Pcap=sw46}, --8
    Team= {Aexp=nw,   Sced=nw,  Site=nw}, --6
    Time= {Acap=sw46, Pcap=sw46, Tool=sw26}, --10
    Tool= {Acap=nw,   Pcap=nw,  Pmat=nw}} end -- 6

function from(lo,hi)     
  lo = lo or 1
  hi = hi or 5
  return lo+(hi-lo)*math.random() end

function within(z,lo,hi) 
  lo = lo or 1
  hi = hi or 5
  return (0.5 + (z>=lo and z<=hi) and z or lo+z%(hi-lo)) // 1 end

function Cocomo.risk(x,    n)
  n=0
  for a1,t in pairs(Cocomo.risks()) do
    for a2,d in pairs(t) do n = n + d[x[a1]][x[a2]] end end
  return (n/108)^0.33 end

function Cocomo.go(project)-- project has same format as meta, but maybe fewer slos
  local w,x,y = {},{},{}
  for k,t0 in pairs(Cocomo.meta) do
    if k=="Loc" then y.loc = from(2,200) else
      t1   = project[k] or t0
      x[k] = within( from(t1[2], t1[3]), t0[2], t0[3] ) 
      w[k] = Cocomo.mxplusb(t0[1], x[k]) end
  end
  y.AX         = from(2.3, 9.18)
  y.BX         = (.85 - 1.1)/(9.18-2.2)*y.A+.9+(1.2-.8)/2
  y["Effort-"] = Cocomo.effort(w, y)
  y["Risk-"]   = Cocomo.risk(x)
  return x,y end

-------------------------------------------------------------------------------
function o(x,     f,g,fmt) --> (any) --> str
  fmt= string.format
  f= function(x) return #x>0 and map(x,o) or sort(kap(x,g)) end
  g= function(k,v) if k ~= "is" then return fmt(":%s %s",k,o(x[k])) end end
  return type(x)=="number" and fmt("%.2g",x) or  
         type(x)~="table"  and tostring(x) or 
         (x.is or "") .. "(" .. table.concat(f(x)," ") .. ")" end 

function kap(t,f,   u) --> (list,func) --> t
  u={}; for k,v in pairs(t) do u[1+#u]=f(k,v) end; return u end

function map(t,f,   u) --> (list,func) --> t
  u={}; for _,v in pairs(t) do u[1+#u]=f(  v) end; return u end

function sort(t,fn) --> (list,func) --> list
  table.sort(t,fn); return t end

-------------------------------------------------------------------------------
keys={}; for k,_ in pairs(Cocomo.defaults().x) do push(keys,k) end; keys=sort(keys)
print(o(keys))
for i=1,20 do 
  c = Cocomo.go()
  print(string.format("%.3g \t%.3f",c.y["Risk-"], c.y["Effort-"])) end

-- asdas
