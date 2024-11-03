
function SYM:like(x,prior)
  return  ((self.has[x] or 0) + the.m*prior) / (self.n + the.m)  end

function NUM:like(x,_,      v,tmp)
  v = self.sd^2 + 1/big
  tmp = exp(-1*(x - self.mu)^2/(2*v)) / (2*pi*v) ^ 0.5
  return max(0,min(1, tmp + 1/big)) end

function DATA:loglike(row, nall, nh,          prior,F,G)
  prior = (#self.rows + the.k) / (nall + the.k*nh)
  F     = function(x) return L( x:like(row[x.at], prior) ) end
  L     = function(n) return n>0 and log(n) or 0 end
  return L(prior) + l.sum(self.cols.x, F) end

function DATA:acquire()
  local Y,B,R,BR,test,train,todo,done,best,rest,n,_
  Y  = function(r) return self:ydist(r) end
  B  = function(r) return best:loglike(r, #done, 2) end
  R  = function(r) return rest:loglike(r, #done, 2) end
  BR = function(r) return B(r) - R(r) end
  n  = min(500, the.Trainings * #self.rows)
  train,test = l.split(l.shuffle(self.rows), n)
  test, _    = l.split(test, min(500,#test))
  done,todo  = l.split(train, the.start)            --- [1]
  while true do
    done = l.keysort(done,Y)
    if #done > the.Stop or #todo < 5 then break end --- [6]
    best,rest = l.split(done, sqrt(#done))          --- [2]
    best, rest = self:clone(best), self:clone(rest)  --- [3]
    todo = l.keysort(todo,BR)                       --- [4]
    for _=1,2 do                                    --- [5]
      l.push(done, table.remove(todo)); 
      l.push(done, table.remove(todo,1)) end end
  return done[1], l.keysort(test,BR)[#test] end     --- [7]
