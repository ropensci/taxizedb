#' src - dplyr src objects
#'
#' @name src_taxizedb
#' @param user (character) user name
#' @param password (character) password
#' @param dbname (character) database name. Defaults: ITIS, col, and
#' plantlistdb for ITIS, COL, and ThePlantlist, respectively. GBIF uses
#' SQLite so doesn't have a database name
#' @param path (character) path to SQLite database
#' @param ... Further args passed on to [DBI::dbConnect()]
#' @return an src object
#' @examples \dontrun{
#' # src_itis()
#' # src_tpl()
#' # src_col()
#' # src_gbif()
#' }

#' @export
#' @rdname src_taxizedb
src_itis <- function(user, password, dbname = "ITIS", ...) {
  con <- DBI::dbConnect(RPostgreSQL::PostgreSQL(),
                        dbname = dbname, user = user, password = password)
  dbplyr::src_dbi(con)
}

#' @export
#' @rdname src_taxizedb
src_tpl <- function(user, password, dbname = "plantlist", ...) {
  con <- DBI::dbConnect(RPostgreSQL::PostgreSQL(),
                        dbname = dbname, user = user, password = password)
  dbplyr::src_dbi(con)
}

#' @export
#' @rdname src_taxizedb
src_col <- function(user = "root", password = NULL, dbname = "col", ...) {
  con <- DBI::dbConnect(RMySQL::MySQL(),
                        dbname = dbname, user = user, password = password, ...)
  dbplyr::src_dbi(con)
}

#' @export
#' @rdname src_taxizedb
src_gbif <- function(path) {
  stopifnot(file.exists(path))
  con <- DBI::dbConnect(RSQLite::SQLite(), dbname = path)
  dbplyr::src_dbi(con)
}
