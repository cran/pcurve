.First.lib <- function(lib,pkg)
  {
    library.dynam("pcurve",pkg,lib)
    if(is.R())
      {
        library(stats)
        library(MASS)
      }
 }
