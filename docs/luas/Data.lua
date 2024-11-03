
function SYM:new(txt,at) 
  return l.new(SYM, {at=at, txt=txt, n=0, has={}, most=0, mode=nil}) end

function SYM:add(x)
  if x~="?" then
    self.n = self.n + 1
    self.has[x] = 1 + (self.has[x] or 0) 
    if self.has[x] > self.most then self.most, self.mode = self.has[x], x end end end

function NUM:new(txt, at) 
  return l.new(NUM, {at=at, txt=txt, n=0, mu=0, m2=0, sd=0, lo=big, hi= -big,
                   goal = (txt or ""):find"-$" and 0 or 1}) end

function NUM:add(x)
  if x~="?" then
    self.n = self.n + 1
    local d = x - self.mu
    self.mu = self.mu + d / self.n
    self.m2 = self.m2 + d * (x - self.mu)
    self.sd = self.n < 2 and 0 or (self.m2/(self.n - 1))^.5
    if x > self.hi then self.hi = x end
    if x < self.lo then self.lo = x end end end
 

function NUM:norm(x)
  return x=="?" and x or (x - self.lo)/(self.hi - self.lo) end

function COLS:new(names)
  local all,x,y = {},{},{}
  for at,txt in pairs(names) do 
    l.push(all, (txt:find"^[A-Z]" and NUM or SYM):new(txt,at))
    if not txt:find"X$" then
      l.push(txt:find"[!+-]$" and y or x, all[#all]) end end
  return l.new(COLS, {names=names, all=all, x=x, y=y}) end

function COLS:add(row)
  for _,col in pairs(self.all) do col:add(row[col.at]) end 
  return row end

function DATA:new()
  return l.new(DATA, {cols=nil, rows={}})  end

function DATA:read(file)
  for row in l.csv(file) do self:add(row) end; return self end

function DATA:adds(t)
  for _,row in pairs(t or {}) do self:add(row) end; return self end

function DATA:add(row) 
  if self.cols then l.push(self.rows,  self.cols:add(row)) else self.cols=COLS:new(row) end 
  return self end 

function DATA:clone(rows,     d) 
  return DATA:new():add(self.cols.names):adds(rows) end
   
