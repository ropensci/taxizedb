tdb_cache <- NULL

.onLoad <- function(libname, pkgname){
  x <- hoardr::hoard()
  x$cache_path_set("taxizedb")
  tdb_cache <<- x
}
