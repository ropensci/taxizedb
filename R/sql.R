#' ITIS sql query
#'
#' @keywords internal
itis_SQL <- function(query, user = NULL, password = NULL, ...){
  # initialize connection to database
  itis_db <- dplyr::src_postgres(dbname = "ITIS", user = user, password = password, ...)
  # query, return data.frame
  dplyr::tbl(itis_db, dplyr::sql(query)) %>% dplyr::collect() %>% data.frame
}

#' COL sql initiation and query
#'
#' @keywords internal
col_SQL <- function(query, user = "root", password = NULL, ...){
  # initialize connection to database
  col_db <- dplyr::src_mysql(dbname = "col", user = user, password = password, ...)
  # query, return data.frame
  dplyr::tbl(col_db, dplyr::sql(query)) %>% dplyr::collect() %>% data.frame
}

#' ThePlantList sql initiation and query
#'
#' @keywords internal
plantlist_SQL <- function(query, user = NULL, password = NULL, ...){
  # initialize connection to database
  tpl_db <- dplyr::src_postgres(dbname = "plantlistdb", user = user, password = password, ...)
  # query, return data.frame
  dplyr::tbl(tpl_db, dplyr::sql(query)) %>% dplyr::collect() %>% data.frame
}
