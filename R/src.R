#' src - dplyr src objects
#'
#' @name src_taxizedb
#' @param user (character) user name
#' @param password (character) password
#' @param dbname (character) database name. Defaults: ITIS, col, and
#' plantlistdb for ITIS, COL, and ThePlantlist, respectively
#' @param ... Further args passed on to [dplyr::src_postgres()] or
#' [dplyr::src_mysql()]
#' @return an src object
#' @examples \dontrun{
#' src_itis()
#' src_tpl()
#' src_col()
#' }

#' @export
#' @rdname src_taxizedb
src_itis <- function(user = NULL, password = NULL, dbname = "ITIS", ...){
  dplyr::src_postgres(dbname = dbname, user = user, password = password, ...)
}

#' @export
#' @rdname src_taxizedb
src_tpl <- function(user = NULL, password = NULL, dbname = "plantlist", ...){
  dplyr::src_postgres(dbname = dbname, user = user, password = password, ...)
}

#' @export
#' @rdname src_taxizedb
src_col <- function(user = "root", password = NULL, dbname = "col", ...){
  dplyr::src_mysql(dbname = dbname, user = user, password = password, ...)
}
