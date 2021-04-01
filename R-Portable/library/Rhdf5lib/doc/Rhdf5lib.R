## ----headers---------------------------------------------------------------
system.file(package="Rhdf5lib", "include")

## ---- zlib-path, eval = FALSE----------------------------------------------
#  BiocManager::install('Rhdf5lib', configure.args = "--with-zlib='/path/to/zlib/'")

## ----sessionInfo, echo=FALSE-----------------------------------------------
sessionInfo()

