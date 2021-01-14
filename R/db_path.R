#' database path
#' @export
#' @param db (character) db name. one of: itis, tpl, col, gbif,
#' ncbi, wikidata, wfo. required
db_path <- function(db) {
  file <- switch(
    db,
    itis = "ITIS.sqlite",
    tpl = "plantlist.sqlite",
    col = "col.sqlite",
    gbif = "gbif.sqlite",
    ncbi = "NCBI.sql",
    wikidata = "wikidata.sqlite",
    wfo = "wfo.sqlite",
    stop("must be one of itis, tpl, col, gbif, ncbi, wikidata, wfo",
      call. = FALSE)
  )
  file.path(tdb_cache$cache_path_get(), file)
}
