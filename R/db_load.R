#' Load taxonomic databases
#'
#' @export
#' @name db_load
#' @param path (character) path to the \code{.sql} database file
#' @param user (character) User name
#' @param pwd (character) Password, if any
#' @param verbose (logical) Print messages. Default: \code{TRUE}
#'
#' @return Nothing, just message on success
#' @details These functions load SQL DBs into the respective driver,
#' and they return the file path, but they don't load the database
#'
#' @section Supported:
#' \itemize{
#'  \item ITIS - PostgreSQL
#'  \item the PlantList - PostgreSQL
#'  \item Catalogue of Life - MySQL
#' }
#'
#' @section Beware:
#' COL database loading takes a long time, e.g., 30 minutes. you may
#' want to run it in a separate R session, or just look at the db_load_col fxn
#' and run the commands in your shell.
#'
#' @examples \dontrun{
#' # ITIS
#' #db_download_itis() %>% db_load_itis()
#' x <- db_download_itis()
#' db_load_itis(x)
#'
#' # the plant list
#' #db_download_tpl() %>% db_load_tpl()
#' x <- db_download_tpl()
#' db_load_tpl(x)
#'
#' # catalogue of life
#' #db_download_col() %>% db_load_col()
#' x <- db_download_col()
#' db_load_col(x)
#' }

#' @export
#' @rdname db_load
db_load_itis <- function(path, user, pwd = NULL, verbose = TRUE){
  mssg(verbose, "loading database...")
  system(sprintf("psql %s %s -f %s", cl("-U ", user), cl("-p ", pwd), path))
  mssg(verbose, "Done. see ?src_itis")
}

#' @export
#' @rdname db_load
db_load_tpl <- function(path, user = NULL, pwd = NULL, verbose = TRUE){
  mssg(verbose, 'creating PostgreSQL database...')
  drv <- dbDriver("PostgreSQL")
  psqlconn <- if (is.null(pwd)) {
    dbConnect(drv, user = user)
  } else {
    dbConnect(drv, user = user, password = pwd)
  }
  dbSendQuery(psqlconn, "CREATE DATABASE plantlist;")
  system(sprintf("psql %s %s plantlist < %s", cl("-U ", user), cl("-p ", pwd), path))
  invisible(dbDisconnect(psqlconn))
  mssg(verbose, "Done. see ?src_tpl")
}

#' @export
#' @rdname db_load
db_load_col <- function(path, user = "root", pwd = NULL, verbose = TRUE){
  mssg(verbose, 'creating MySQL database, this may take a while, get some coffee...')
  system(sprintf("mysql %s %s -e 'CREATE DATABASE IF NOT EXISTS col';", cl("-u ", user), cl("-p ", pwd)))
  system(sprintf("mysql %s %s col < %s", cl("-u ", user), cl("-p ", pwd), path))
  mssg(verbose, "Done. see ?src_col")
}

## some code tried to use to convert COL mysql db to postgresql, didn't work
# mysqldump --compatible=postgresql --default-character-set=utf8 -r col.mysql -u root col2014ac
# python db_converter.py col.mysql col.psql
# psql -f col.psql
