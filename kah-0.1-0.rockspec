-- luarocks install my_script-1.0-1.rockspec

package = "kah"
version = "0.1-0"
source = {
   url = "https://raw.githubusercontent.com/timm/kah/refs/heads/main/src/kah.lua"
}
description = {
   summary = "Incremental, explainable AI. Use current model to decide what to query next."
   detailed = "Sequential model optimization. TPE using a Bayes classifier"
   license = "MIT",
   homepage = "https://timm.github.io/kah/"
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

