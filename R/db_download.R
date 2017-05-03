#' Download taxonomic databases
#'
#' @export
#' @name db_download
#' @param verbose (logical) Print messages. Default: `TRUE`
#'
#' @return Path to the downloaded SQL database
#' @details Downloads sql database, cleans up unneeded files, returns path
#' to sql file
#'
#' @section Supported:
#' \itemize{
#'  \item ITIS - PostgreSQL
#'  \item The PlantList - PostgreSQL
#'  \item Catalogue of Life - MySQL
#'  \item GBIF - SQLite
#' }
#'
#' @seealso [tdb_cache]
#'
#' @section Beware:
#' COL database loading takes a long time, e.g., 30 minutes. you may
#' want to run it in a separate R session, or just look at the db_load_col fxn
#' and run the commands in your shell.
#'
#' @examples \dontrun{
#' # ITIS
#' # x <- db_download_itis()
#' # db_load_itis(x)
#' # src_itis()
#'
#' # Plantlist
#' # x <- db_download_tpl()
#' # db_load_tpl(x, "sacmac")
#' # src_tpl()
#'
#' # COL
#' # x <- db_download_col()
#' # db_load_col(x)
#' # src_col()
#'
#' # GBIF
#' # x <- db_download_gbif()
#' # db_load_gbif()
#' # src_gbif(x)
#' }

#' @export
#' @rdname db_download
db_download_itis <- function(verbose = TRUE){
  # paths
  db_url <- 'https://www.itis.gov/downloads/itisPostgreSql.zip'
  db_path <- file.path(tdb_cache$cache_path_get(), 'itisPostgreSql.zip')
  db_path_file <- file.path(tdb_cache$cache_path_get(), 'itisPostgreSql')
  final_file <- file.path(tdb_cache$cache_path_get(), 'ITIS.sql')
  # make home dir if not already present
  tdb_cache$mkdir()
  # download data
  mssg(verbose, 'downloading...')
  curl::curl_download(db_url, db_path, quiet = TRUE)
  # unzip
  mssg(verbose, 'unzipping...')
  utils::unzip(db_path, exdir = db_path_file)
  # get file path
  dirs <- list.dirs(db_path_file, full.names = TRUE)
  dir_date <- dirs[ dirs != db_path_file ]
  db_path <- list.files(dir_date, pattern = ".sql", full.names = TRUE)
  # move database
  file.rename(db_path, final_file)
  # cleanup
  mssg(verbose, 'cleaning up...')
  unlink(db_path)
  unlink(db_path_file, recursive = TRUE)
  # return path
  return(final_file)
}

#' @export
#' @rdname db_download
db_download_tpl <- function(verbose = TRUE){
  # paths
  db_url <- 'https://github.com/ropensci/taxizedbs/blob/master/theplantlist/plantlist.zip?raw=true' #nolint
  db_path <- file.path(tdb_cache$cache_path_get(), 'plantlist.zip')
  db_path_file <- file.path(tdb_cache$cache_path_get(), 'plantlist')
  final_file <- file.path(tdb_cache$cache_path_get(), 'plantlist.sql')
  # make home dir if not already present
  tdb_cache$mkdir()
  # download data
  mssg(verbose, 'downloading...')
  curl::curl_download(db_url, db_path, quiet = TRUE)
  # unzip
  mssg(verbose, 'unzipping...')
  utils::unzip(db_path, exdir = db_path_file)
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
#' @rdname db_download
db_download_col <- function(verbose = TRUE){
  # paths
  db_url <- 'http://www.catalogueoflife.org/services/res/col2015ac_linux.tar.gz'
  db_path <- file.path(tdb_cache$cache_path_get(), 'col2015ac_linux.tar.gz')
  db_path_file <- file.path(tdb_cache$cache_path_get(), 'colmysql')
  db_sql_path <- file.path(tdb_cache$cache_path_get(),
                           '/colmysql/col2015ac_linux/col2015ac.sql.tar.gz')
  db_sql_out <- file.path(tdb_cache$cache_path_get(),
                          'colmysql/col2015ac_linux')
  final_file <- file.path(tdb_cache$cache_path_get(), 'col.sql')
  # make home dir if not already present
  tdb_cache$mkdir()
  # download data
  mssg(verbose, 'downloading...')
  curl::curl_download(db_url, db_path, quiet = TRUE)
  # unzip
  mssg(verbose, 'unzipping...')
  #unzip(db_path, exdir = db_path_file)
  utils::untar(db_path, exdir = db_sql_out)
  utils::untar(db_sql_path, exdir = db_sql_out)
  # move database
  file.rename(file.path(db_sql_out, "col2015ac.sql"), final_file)
  # cleanup
  mssg(verbose, 'cleaning up...')
  unlink(db_path)
  unlink(db_path_file, recursive = TRUE)
  return( final_file )
}

#' @export
#' @rdname db_download
db_download_gbif <- function(verbose = TRUE){
  db_url <- 'https://s3-us-west-2.amazonaws.com/gbif-backbone/gbif.sqlite'
  db_path <- file.path(tdb_cache$cache_path_get(), 'gbif.sqlite')
  final_file <- file.path(tdb_cache$cache_path_get(), 'gbif.sqlite')
  tdb_cache$mkdir()
  mssg(verbose, 'downloading...')
  curl::curl_download(db_url, db_path, quiet = TRUE)
  mssg(verbose, 'all done...')
  return(db_path)
}
