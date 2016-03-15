#' Download and load taxonomic databases
#'
#' @export
#' @name db
#' @param path (character) path to the \code{.sql} database file
#' @param user (character) User name
#' @param pwd (character) Password, if any
#' @param verbose (logical) Print messages. Default: \code{TRUE}
#'
#' @return Downloads sql database, loads it into PostgreSQL or MySQL,
#' cleans up unneeded files, returns path of sql DB
#' @details Supported:
#' \itemize{
#'  \item ITIS - PostgreSQL
#'  \item the PlantList - PostgreSQL
#'  \item Catalogue of Life - MySQL
#' }
#'
#' \code{db_download*()} functions are for downloading SQL DBs, and they return
#' the file path, but they don't load the database
#'
#' \code{db_load*()} functions are for loading SQL DBs into the respective driver,
#' and they return the file path, but they don't load the database
#'
#' Beware: COL database loading takes a long time, e.g., 30 minutes. you may
#' want to run it in a separate R session, or just look at the db_load_col fxn
#' and run the commands in your shell.
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
#' @rdname db
db_download_itis <- function(verbose = TRUE){
  # paths
  itis_db_url <- 'http://www.itis.gov/downloads/itisPostgreSql.zip'
  itis_db_path <- path.expand('~/.taxize_local/itisPostgreSql.zip')
  itis_db_path_file <- path.expand('~/.taxize_local/itisPostgreSql')
  itis_final_file <- path.expand('~/.taxize_local/ITIS.sql')
  # download data
  mssg(verbose, 'downloading...')
  curl::curl_download(itis_db_url, itis_db_path, quiet = TRUE)
  # unzip
  mssg(verbose, 'unzipping...')
  unzip(itis_db_path, exdir = itis_db_path_file)
  # get file path
  dirs <- list.dirs(itis_db_path_file, full.names = TRUE)
  dir_date <- dirs[ dirs != itis_db_path_file ]
  db_path <- list.files(dir_date, pattern = ".sql", full.names = TRUE)
  # move database
  file.rename(db_path, itis_final_file)
  # cleanup
  mssg(verbose, 'cleaning up...')
  unlink(itis_db_path)
  unlink(itis_db_path_file, recursive = TRUE)
  # return path
  return( itis_final_file )
}

#' @export
#' @rdname db
db_load_itis <- function(path, user = "sacmac", pwd = NULL, verbose = TRUE){
  mssg(verbose, "loading database...")
  system(sprintf("psql %s %s -f %s", cl("-U ", user), cl("-p ", pwd), path))
  mssg(verbose, "Done. see ?src_itis")
}

#' @export
#' @rdname db
db_download_tpl <- function(verbose = TRUE){
  # paths
  db_url <- 'https://github.com/ropensci/taxizedbs/blob/master/theplantlist/plantlist.zip?raw=true'
  db_path <- path.expand('~/.taxize_local/plantlist.zip')
  db_path_file <- path.expand('~/.taxize_local/plantlist')
  final_file <- path.expand('~/.taxize_local/plantlist.sql')
  # download data
  mssg(verbose, 'downloading...')
  curl::curl_download(db_url, db_path, quiet = TRUE)
  # unzip
  mssg(verbose, 'unzipping...')
  unzip(db_path, exdir = db_path_file)
  # move database
  file.rename(file.path(db_path_file, "plantlist.sql"), final_file)
  # cleanup
  mssg(verbose, 'cleaning up...')
  unlink(db_path)
  unlink(db_path_file, recursive = TRUE)
  # return path
  return( final_file )
}

#' @export
#' @rdname db
db_load_tpl <- function(path, user = NULL, pwd = NULL, verbose = TRUE){
  mssg(verbose, 'creating PostgreSQL database...')
  drv <- dbDriver("PostgreSQL")
  psqlconn <- dbConnect(drv, user = "sacmac")
  dbSendQuery(psqlconn, "CREATE DATABASE plantlist;")
  system(sprintf("psql %s %s plantlist < %s", cl("-U ", user), cl("-p ", pwd), path))
  mssg(verbose, "Done. see ?src_tpl")
}

#' @export
#' @rdname db
db_download_col <- function(verbose = TRUE){
  # paths
  #db_url <- 'http://www.catalogueoflife.org/services/res/AnnualChecklist2013-Linux.zip'
  db_url <- 'http://www.catalogueoflife.org/services/res/col2015ac_linux.tar.gz'
  db_path <- path.expand('~/.taxize_local/col2015ac_linux.tar.gz')
  db_path_file <- path.expand('~/.taxize_local/colmysql')
  db_sql_path <- path.expand('~/.taxize_local/colmysql/col2015ac_linux/col2015ac.sql.tar.gz')
  db_sql_out <- path.expand('~/.taxize_local/colmysql/col2015ac_linux')
  final_file <- path.expand('~/.taxize_local/col.sql')
  # download data
  mssg(verbose, 'downloading...')
  curl::curl_download(db_url, db_path, quiet = TRUE)
  # unzip
  mssg(verbose, 'unzipping...')
  #unzip(db_path, exdir = db_path_file)
  untar(db_path, exdir = db_sql_out)
  untar(db_sql_path, exdir = db_sql_out)
  # move database
  file.rename(file.path(db_sql_out, "col2015ac.sql"), final_file)
  # cleanup
  mssg(verbose, 'cleaning up...')
  unlink(db_path)
  unlink(db_path_file, recursive = TRUE)
  return( final_file )
}

#' @export
#' @rdname db
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
