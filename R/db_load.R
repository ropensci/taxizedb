#' Load taxonomic databases
#'
#' @export
#' @name db_load
#' @param path (character) path to the `.sql` database file
#' @param user (character) User name
#' @param pwd (character) Password, if any
#' @param verbose (logical) Print messages. Default: `TRUE`
#'
#' @return Nothing, just message on success
#' @details These functions load SQL DBs into the respective driver,
#' and they return the file path, but they don't load the database
#'
#' We check if the database used for each source is installed on
#' your machine first. and if it is running or not, with errors
#' for the user if neither are true.
#'
#' @section Supported:
#' \itemize{
#'  \item ITIS - PostgreSQL
#'  \item the PlantList - PostgreSQL
#'  \item Catalogue of Life - MySQL
#'  \item GBIF - SQLite
#' }
#'
#'
#' @section Beware:
#' COL database loading takes a long time, e.g., 30 minutes. you may
#' want to run it in a separate R session, or just look at the db_load_col fxn
#' and run the commands in your shell.
#'
#' @examples \dontrun{
#' # ITIS
#' # x <- db_download_itis()
#' # db_load_itis(x, "<your user name>", "<your password>")
#'
#' # Plantlist
#' # x <- db_download_tpl()
#' # db_load_tpl(x, "<your user name>", "<your password>")
#'
#' # COL
#' x <- db_download_col()
#' db_load_col(x, "<your user name>", "<your password>")
#'
#' # GBIF
#' ## only checks if sqlite installed
#' db_load_gbif()
#' }

#' @export
#' @rdname db_load
db_load_itis <- function(path, user, pwd = NULL, verbose = TRUE) {
  mssg(verbose, 'checking if `path` exists...')
  stopifnot(file.exists(path))

  mssg(verbose, 'checking if Postgres installed...')
  db_installed("psql")
  mssg(verbose, "loading database...")

  mssg(verbose, "checking if Postgres is running...")
  if (is.null(pwd)) {
    psqlconn <- tryCatch(
      DBI::dbConnect(RPostgreSQL::PostgreSQL(), user = user), error = function(e) e)
  } else {
    psqlconn <- tryCatch(
      DBI::dbConnect(RPostgreSQL::PostgreSQL(), user = user, password = pwd),
      error = function(e) e)
  }
  if (inherits(psqlconn, "error")) {
    stop("Make sure Postgres is on/running\n  ", psqlconn$message)
  }
  if (is.null(pwd)) {
    cmd <- sprintf("psql %s -f %s", cl("-U ", user), path)
  } else {
    cmd <- sprintf("PGPASSWORD=[%s] psql %s -f %s", pwd,
                   cl("-U ", user), path)
  }
  system(cmd)
  invisible(DBI::dbDisconnect(psqlconn))
  mssg(verbose, "Done. see ?src_itis")
}

#' @export
#' @rdname db_load
db_load_tpl <- function(path, user, pwd = NULL, verbose = TRUE) {
  mssg(verbose, 'checking if `path` exists...')
  stopifnot(file.exists(path))

  mssg(verbose, 'checking if Postgres installed...')
  db_installed("psql")
  mssg(verbose, 'creating Postgres database...')

  mssg(verbose, "checking if Postgres is running...")
  if (is.null(pwd)) {
    psqlconn <- tryCatch(
      DBI::dbConnect(
        RPostgreSQL::PostgreSQL(), user = user), error = function(e) e)
  } else {
    psqlconn <- tryCatch(
      DBI::dbConnect(RPostgreSQL::PostgreSQL(), user = user, password = pwd),
      error = function(e) e)
  }
  if (inherits(psqlconn, "error")) {
    stop("Make sure Postgres is on/running\n  ", psqlconn$message)
  }
  # drop database if exists
  DBI::dbSendQuery(psqlconn, "DROP DATABASE IF EXISTS plantlist;")
  # create database
  DBI::dbSendQuery(psqlconn, "CREATE DATABASE plantlist;")
  if (is.null(pwd)) {
    cmd <- sprintf("psql %s plantlist < %s", cl("-U ", user), path)
  } else {
    cmd <- sprintf("PGPASSWORD=[%s] psql %s plantlist < %s", pwd,
            cl("-U ", user), path)
  }
  system(cmd)
  invisible(DBI::dbDisconnect(psqlconn))
  mssg(verbose, "Done. see ?src_tpl")
}

#' @export
#' @rdname db_load
db_load_col <- function(path, user = "root", pwd = NULL, verbose = TRUE) {
  mssg(verbose, 'checking if `path` exists...')
  stopifnot(file.exists(path))

  mssg(verbose, 'checking if MySQL installed...')
  db_installed("mysql")

  mssg(verbose, "checking if MySQL is running...")

  if (is.null(pwd)) {
    mysqlconn <- tryCatch(
      DBI::dbConnect(RMySQL::MySQL(), user = user), error = function(e) e)
  } else {
    mysqlconn <- tryCatch(
      DBI::dbConnect(RMySQL::MySQL(), user = user, password = pwd),
      error = function(e) e)
  }
  if (inherits(mysqlconn, "error")) {
    stop("Make sure MySQL is on/running\n  ", mysqlconn$message)
  }
  invisible(DBI::dbDisconnect(mysqlconn))

  mssg(
    verbose,
    'creating MySQL database, this may take a while, get some coffee...')
  system(sprintf("mysql %s %s -e 'CREATE DATABASE IF NOT EXISTS col';",
                 cl("-u ", user), cl("-p ", pwd)))
  system(sprintf("mysql %s %s col < %s", cl("-u ", user), cl("-p ", pwd),
                 path))
  mssg(verbose, "Done. see ?src_col")
}

#' @export
#' @rdname db_load
db_load_gbif <- function(verbose = TRUE) {
  mssg(verbose, 'checking if SQLite installed...')
  db_installed("sqlite3")
  mssg(verbose, "Done. see ?src_gbif")
}
