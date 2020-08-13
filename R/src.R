#' src - dplyr src objects
#'
#' @name src_taxizedb
#' @param path (character) path to SQLite database. by default
#' we use the function [db_path()] to get the path
#' @param ... Further args passed on to [DBI::dbConnect()]
#' @return an src object
#' @examples \dontrun{
#' # src_itis()
#' # src_tpl()
#' # src_col()
#' # src_gbif()
#' # src_ncbi()
#' # src_wikidata()
#' # src_wfo()
#' }

#' @export
#' @rdname src_taxizedb
src_itis <- function(path = db_path("itis"), ...) {
  stopifnot(file.exists(path))
  con <- DBI::dbConnect(RSQLite::SQLite(), dbname = path, ...)
  dbplyr::src_dbi(con, auto_disconnect=TRUE)
}

#' @export
#' @rdname src_taxizedb
src_tpl <- function(path = db_path("tpl"), ...) {
  stopifnot(file.exists(path))
  con <- DBI::dbConnect(RSQLite::SQLite(), dbname = path, ...)
  dbplyr::src_dbi(con, auto_disconnect=TRUE)
}

#' @export
#' @rdname src_taxizedb
src_col <- function(path = db_path("col"), ...) {
  stopifnot(file.exists(path))
  con <- DBI::dbConnect(RSQLite::SQLite(), dbname = path, ...)
  dbplyr::src_dbi(con, auto_disconnect=TRUE)
}

#' @export
#' @rdname src_taxizedb
src_gbif <- function(path = db_path("gbif"), ...) {
  stopifnot(file.exists(path))
  con <- DBI::dbConnect(RSQLite::SQLite(), dbname = path, ...)
  dbplyr::src_dbi(con)
}

#' @export
#' @rdname src_taxizedb
src_ncbi <- function(path = db_path("ncbi"), ...) {
  stopifnot(file.exists(path))
  con <- RSQLite::dbConnect(RSQLite::SQLite(), dbname = path, ...)
  dbplyr::src_dbi(con, auto_disconnect=TRUE)
}

#' @export
#' @rdname src_taxizedb
src_wikidata <- function(path = db_path("wikidata"), ...) {
  stopifnot(file.exists(path))
  con <- RSQLite::dbConnect(RSQLite::SQLite(), dbname = path, ...)
  dbplyr::src_dbi(con, auto_disconnect=TRUE)
}

#' @export
#' @rdname src_taxizedb
src_wfo <- function(path = db_path("wfo"), ...) {
  stopifnot(file.exists(path))
  con <- RSQLite::dbConnect(RSQLite::SQLite(), dbname = path, ...)
  dbplyr::src_dbi(con, auto_disconnect=TRUE)
}
