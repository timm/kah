
function l.tests(eg,tests,      FN,_)
  FN = function(x,     ok,msg,bad) 
         math.randomseed(the.rseed)
         ok,msg = xpcall(eg[x], debug.traceback, _)
         bad = ok==false or msg==false
         print((bad and l.red" FAIL " or l.green" PASS ") .." on "..x)
         return bad and 1 or 0 end
  os.exit(l.sum(tests, FN)) end
help:gsub("%s+-%S%s(%S+)[^=]+=%s+(%S+)%s*\n", function(k,v) the[k]= l.coerce(v) end)
if l.o(arg):find"kah.lua" then
  the = l.cli(the)
  if the.help then os.exit(print(help)) end
  math.randomseed(the.rseed or 1)
  l.map(arg, function(s) if EG[s:sub(3)] then EG[s:sub(3)]() end end) end
return {SYM=SYM, NUM=NUM, COLS=COLS, DATA=DATA, the=the, help=help, lib=l}
