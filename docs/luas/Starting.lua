
local l,the,big,help = {},{},1E32,[[
kah.lua : how to change your mind (using TPE + Bayes classifier)
(c) 2024 Tim Menzies (timm@ieee.org). MIT license.
USAGE: 
  chmod +x kah.lua
  ./kah.lua [OPTIONS]
OPTIONS:
  -B Bootstraps num.of bootstraps        = 256
  -c conf       statistical confidence   = .05
  -C Cohen      Cohen's threshold        = .35
  -d delta      Cliff's delta            = .195
  -h help       show help                = false
  -k k          bayes control            = 1
  -m m          bayes control            = 2
  -p p          distance coeffecient     = 2
  -r rseed      random seed              = 1234567891
  -s start      init number of samples   = 4
  -S Stop       max number of labellings = 30
  -t train      data                     = ../test/auto93.csv 
  -T Trainings  max size train set       = .33
]]

local SYM, NUM, DATA, COLS, SOME = {}, {}, {}, {}, {}   

local abs, cos, exp, log    = math.abs, math.cos, math.exp, math.log

local max, min, pi, R, sqrt = math.max, math.min, math.pi, math.random, math.sqrt
      
