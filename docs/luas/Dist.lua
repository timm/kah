
function DATA:ydist(row, D)
  D = function(y) return (abs(y:norm(row[y.at]) - y.goal))^the.p end
  return (l.sum(self.cols.y,D) /#self.cols.y)^(1/the.p) end

function SYM:dist(a,b)
  return (a=="?" and b=="?" and 0) or (a==b and 0 or 1) end

function NUM:dist(a,b)
  if a=="?" and b=="?" then return 1 end
  a,b = self:norm(a), self:norm(b)
  a = a ~= "?" and a or (b<0.5 and 1 or 0)
  b = b ~= "?" and b or (a<0.5 and 1 or 0)
  return abs(a-b) end

function DATA:xdist(row1,row2,    d,n) -- (list) -> number
  D = function(x) return x:dist(row1[x.at], row2[x.at])^the.p  end
  return (l.sum(self.cols.x, D) / #self.cols.x) ^ (1/the.p) end

function DATA:neighbors(row,rows)
  return l.keysort(rows or self.rows, function(r) return self:xdist(r,row) end) end
