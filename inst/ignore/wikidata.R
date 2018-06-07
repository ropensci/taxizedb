db_download_wikidata <- function(verbose = TRUE){
  # FIXME: make sure to update the URL below
  db_url <- 'https://zenodo.org/record/1213477/files/wikidata-taxon-info20171227.tsv.gz'
  
  txt_file <- file.path(tdb_cache$cache_path_get(), 'wikidata-taxon-info20171227.tsv.gz')
  final_file <- file.path(tdb_cache$cache_path_get(), 'wikidata.sqlite')

  if (file.exists(final_file)) {
    mssg(verbose, "Database already exists, returning old file")
    return(final_file)
  }

  tdb_cache$mkdir()

  # download
  mssg(verbose, 'downloading...')
  curl::curl_download(db_url, txt_file, quiet = TRUE)

  # load taxa.txt
  taxa_txt <- readr::read_tsv(txt_file, skip = 1, 
    col_names = c('wikidata_id', 'scientific_name', 'rank_id', 'parent_id', 'external_ids'))

  mssg(verbose, 'building SQLite database...')
  db <- RSQLite::dbConnect(RSQLite::SQLite(), dbname=final_file)

  # Create table
  RSQLite::dbExecute(conn=db, "
    CREATE TABLE wikidata (
      wikidata_id TEXT,
      scientific_name TEXT,
      rank_id TEXT,
      parent_id TEXT,
      external_ids TEXT
    )
    "
  )

  # Load tables
  RSQLite::dbWriteTable(
    conn   = db,
    name   = 'wikidata',
    value  = as.data.frame(taxa_txt),
    append = TRUE
  )

  # Create indices on taxonID columns
  RSQLite::dbExecute(db,
    'CREATE INDEX wikidata_id_index ON wikidata (wikidata_id)'
  )
  RSQLite::dbExecute(db,
    'CREATE INDEX scientific_name_index ON wikidata (scientific_name)'
  )

  RSQLite::dbDisconnect(db)

  # cleanup
  mssg(verbose, 'cleaning up...')
  unlink(txt_file)
  mssg(verbose, 'all done...')
  return(final_file)
}

#' @export
#' @rdname db_load
db_load_wikidata <- function(verbose = TRUE) {
  mssg(verbose, "Done. see ?src_wikidata")
}

#' @export
#' @rdname src_taxizedb
src_wikidata <- function(path) {
  stopifnot(file.exists(path))
  con <- RSQLite::dbConnect(RSQLite::SQLite(), dbname=path)
  dbplyr::src_dbi(con, auto_disconnect=TRUE)
}
