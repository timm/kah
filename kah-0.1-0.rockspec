-- luarocks install my_script-1.0-1.rockspec

package = "kah"
version = "0.1-0"
source = {
   url = "https://gist.githubusercontent.com/timm/5472f90e896d57e7e86611c238942e4e/raw/876d6c8b9257a9fb530c3cd3e9690a7cf559f4c7/ruler.lua"
}
description = {
   summary = "Incremental, explainable AI. Use current model to decide what to query next."
   detailed = "Sequential model optimization. TPE using a Bayes classifier"
   license = "MIT",
   homepage = "https://gist.github.com/timm/5472f90e896d57e7e86611c238942e4e"
}
build = {
   type = "builtin",
   modules = {
      my_script = "kah.lua"
   },
   bin = {
      my_script = "kah.lua"
   }
}

