#' Download taxonomic databases
#'
#' @export
#' @name db
#' @param user User name
#' @param pwd Password
#' @param verbose (logical) Print messages. Default: \code{TRUE}
#' @return Downloads sql database, loads it into PostgreSQL or MySQL,
#' cleans up unneeded files, returns path of sql DB
#' @details Supported:
#' \itemize{
#'  \item ITIS
#'  \item the PlantList
#'  \item Catalogue of Life
#' }
#' @examples \dontrun{
#' db_itis()
#' db_col()
#' db_plantlist()
#' }

#' @export
#' @rdname db
db_itis <- function(verbose = TRUE, user = "sacmac", pwd = NULL){
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
  # load database
  mssg(verbose, "loading database...")
  system(sprintf("psql %s %s -f %s", cl("-U ", user), cl("-p ", pwd), itis_final_file))
  # cleanup
  mssg(verbose, 'cleaning up...')
  unlink(itis_db_path)
  unlink(itis_db_path_file, recursive = TRUE)
  # return path
  return( itis_final_file )
}

#' @export
#' @rdname db
db_plantlist <- function(user = NULL, pwd = NULL, verbose = TRUE){
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
  # load database
  mssg(verbose, 'creating PostgreSQL database...')
  drv <- dbDriver("PostgreSQL")
  psqlconn <- dbConnect(drv, user = "sacmac")
  dbSendQuery(psqlconn, "CREATE DATABASE plantlist;")
  system(sprintf("psql %s %s plantlist < %s", cl("-U ", user), cl("-p ", pwd), final_file))
  # return path
  return( final_file )
}

#' @export
#' @rdname db
db_col <- function(user = "root", pwd = NULL, verbose = TRUE){
  # paths
  db_url <- 'http://www.catalogueoflife.org/services/res/AnnualChecklist2013-Linux.zip'
  db_path <- path.expand('~/.taxize_local/AnnualChecklist2013-Linux.zip')
  db_path_file <- path.expand('~/.taxize_local/colmysql')
  db_sql_path <- path.expand('~/.taxize_local/colmysql/AC2013_linux_users/col2013ac.sql.tar.gz')
  db_sql_out <- path.expand('~/.taxize_local/colmysql/AC2013_linux_users')
  final_file <- path.expand('~/.taxize_local/col.sql')
  # download data
  mssg(verbose, 'downloading...')
  curl::curl_download(db_url, db_path, quiet = TRUE)
  # unzip
  mssg(verbose, 'unzipping...')
  unzip(db_path, exdir = db_path_file)
  untar(db_sql_path, exdir = db_sql_out)
  # move database
  file.rename(file.path(db_sql_out, "col2013ac.sql"), final_file)
  # cleanup
  mssg(verbose, 'cleaning up...')
  unlink(db_path)
  unlink(db_path_file, recursive = TRUE)
  # load mysql database
  mssg(verbose, 'creating MySQL database, this may take a while, get some coffee...')
  system(sprintf("mysql %s %s -e 'CREATE DATABASE IF NOT EXISTS col';", cl("-u ", user), cl("-p ", pwd)))
  system(sprintf("mysql %s %s col < %s", cl("-u ", user), cl("-p ", pwd), final_file))
  # return path
  return( final_file )
}

## some code tried to use to convert COL mysql db to postgresql, didn't work
# mysqldump --compatible=postgresql --default-character-set=utf8 -r col.mysql -u root col2014ac
# python db_converter.py col.mysql col.psql
# psql -f col.psql

cl <- function(x, y){
  if (is.null(y)) {
    ""
  } else {
    paste0(x, y)
  }
}
