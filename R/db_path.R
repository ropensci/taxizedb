#' database path
#' @export
#' @param db (character) db name. one of: itis, tpl, col, gbif,
#' ncbi, wikidata. required
db_path <- function(db) {
  file <- switch(
    db,
    itis = "itis.sqlite",
    tpl = "tpl.sqlite",
    col = "col.sqlite",
    gbif = "gbif.sqlite",
    ncbi = "ncbi.sqlite",
    wikidata = "wikidata.sqlite",
    stop("must be one of itis, tpl, col, gbif, ncbi, wikidata",
      call. = FALSE)
  )
  file.path(tdb_cache$cache_path_get(), file)
}
