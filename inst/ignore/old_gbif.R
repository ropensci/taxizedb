#' @export
#' @rdname db_download
db_download_gbif <- function(verbose = TRUE){
  # paths
  db_url <- 'http://rs.gbif.org/datasets/backbone/backbone-current.zip'
  db_path <- path.expand('~/.taxize_local/backbone-current.zip')
  db_path_file <- path.expand('~/.taxize_local/backbone-current')
  #db_sql_path <- path.expand('~/.taxize_local/gbif/gbif/gbif.sql.tar.gz')
  taxon_file <- path.expand('~/.taxize_local/backbone-current/taxon.txt')
  final_file <- path.expand('~/.taxize_local/gbif.sql')
  # make home dir if not already present
  mkhome(path.expand('~/.taxize_local/'))
  # download data
  mssg(verbose, 'downloading...')
  curl::curl_download(db_url, db_path, quiet = TRUE)
  # unzip
  mssg(verbose, 'unzipping...')
  utils::unzip(db_path, exdir = path.expand('~/.taxize_local'))
  # convert to sqlite database
  #library(DBI)
  con <- dbConnect(RSQLite::SQLite(), path.expand('~/.taxize_local/gbif.sql'))
  ## create table
  RSQLite::dbSendQuery(con, gbif_create_table)
  #RSQLite::dbWriteTable(con, name = "gbif", value = )

  # sql query to load data
  #ldq <- ".mode csv \n .import
  #  /Users/sacmac/.taxize_local/backbone-current/taxon.txt gbif;"
  #RSQLite::dbSendQuery(con, ldq)

  ## load txt file
  system2("sqlite3")
  # move database
  file.rename(file.path(db_sql_out, "gbif.sql"), final_file)
  # cleanup
  mssg(verbose, 'cleaning up...')
  unlink(db_path)
  unlink(db_path_file, recursive = TRUE)
  return( final_file )
}

gbif_create_table <- '
CREATE TABLE gbif (
id integer PRIMARY KEY,
taxonID integer,
datasetID integer,
parentNameUsageID integer,
acceptedNameUsageID integer,
originalNameUsageID integer,
scientificName varchar(300),
taxonRank varchar(50),
nameAccordingTo varchar(50),
namePublishedIn varchar(50),
taxonomicStatus varchar(200),
nomenclaturalStatus varchar(200),
kingdom varchar(200),
phylum varchar(200),
clazz varchar(200),
ordder varchar(200),
family varchar(200),
genus varchar(200),
taxonRemarks text
);
'