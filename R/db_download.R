#' Download taxonomic databases
#'
#' @export
#' @name db_download
#' @param verbose (logical) Print messages. Default: `TRUE`
#' @param overwrite (logical) If `TRUE` force an update by overwriting
#' previously downloaded data. Default: `FALSE`
#' @return (character) path to the downloaded SQL database
#' @details Downloads sql database, cleans up unneeded files, returns path
#' to sql file
#' @seealso [tdb_cache]
#' @examples \dontrun{
#' # ITIS
#' # db_download_itis()
#' # src_itis()
#'
#' # Plantlist
#' # db_download_tpl()
#' # db_download_tpl(overwrite=TRUE) # overwrite - download again
#' # src_tpl()
#'
#' # COL
#' # db_download_col()
#' # src_col()
#'
#' # GBIF
#' # db_download_gbif()
#' # src_gbif()
#'
#' # NCBI
#' # db_download_ncbi()
#' # src_ncbi()
#'
#' # Wikidata
#' # db_download_wikidata()
#' # db_download_wikidata(overwrite=TRUE) # overwrite - download again
#' # src_wikidata()
#' 
#' # World Flora Online
#' # db_download_wfo()
#' # src_wfo()
#' }

#' @export
#' @rdname db_download
db_download_ncbi <- function(verbose = TRUE, overwrite = FALSE) {
  # set paths
  db_url <- 'ftp://ftp.ncbi.nih.gov/pub/taxonomy/taxdmp.zip'
  db_path_file <- file.path(tdb_cache$cache_path_get(), 'taxdump.zip')
  db_path_dir <- file.path(tdb_cache$cache_path_get(), 'taxdump')
  ncbi_names_file <- file.path(db_path_dir, 'names.dmp')
  ncbi_nodes_file <- file.path(db_path_dir, 'nodes.dmp')
  final_file <- file.path(tdb_cache$cache_path_get(), 'NCBI.sql')

  assert(verbose, "logical")
  assert(overwrite, "logical")
  if (file.exists(final_file) && !overwrite) {
    mssg(verbose, "Database already exists, returning old file")
    return(final_file)
  }
  unlink(final_file, force = TRUE)

  # make home dir if not already present
  tdb_cache$mkdir()
  # download data
  mssg(verbose, 'downloading...')
  curl::curl_download(db_url, db_path_file, quiet = TRUE)
  # unzip
  mssg(verbose, 'unzipping...')
  utils::unzip(db_path_file, files = c('names.dmp', 'nodes.dmp'), exdir = db_path_dir)

  # Taxonomy names file (names.dmp):
  #   tax_id       -- the id of node associated with this name
  #   name_txt     -- name itself
  #   unique name  -- the unique variant of this name if name not unique
  #   name class   -- (synonym, common name, ...)
  mssg(verbose, "loading 'names.dmp'...")
  ncbi_names <- readr::read_tsv(
    ncbi_names_file,
    col_names = c("tax_id", "name_txt", "unique_name", "name_class"),
    col_type = "i_c_c_c_",
    quote = ""
  )

  # nodes.dmp file consists of taxonomy nodes. The description for each node includes the following
  #   tax_id                                 -- node id in GenBank taxonomy database
  #   parent_tax_id                          -- parent node id in GenBank taxonomy database
  #   rank                                   -- rank of this node (superkingdom, kingdom, ...)
  #   embl_code                              -- locus-name prefix; not unique
  #   division_id                            -- see division.dmp file
  #   inherited_div_flag            (1 or 0) -- 1 if node inherits division from parent
  #   genetic_code_id                        -- see gencode.dmp file
  #   inherited_GC_flag             (1 or 0) -- 1 if node inherits genetic code from parent
  #   mitochondrial_genetic_code_id          -- see gencode.dmp file
  #   inherited_MGC_flag            (1 or 0) -- 1 if node inherits mitochondrial gencode from parent
  #   GenBank_hidden_flag           (1 or 0) -- 1 if name is suppressed in GenBank entry lineage
  #   hidden_subtree_root_flag      (1 or 0) -- 1 if this subtree has no sequence data yet
  #   comments                               -- free-text comments and citations
  mssg(verbose, "loading 'nodes.dmp'...")
  ncbi_nodes <- readr::read_tsv(
    ncbi_nodes_file,
    col_names=c(
      "tax_id",
      "parent_tax_id",
      "rank",
      "embl_code",
      "division_id",
      "inherited_div_flag",
      "genetic_code_id",
      "inherited_GC_flag",
      "mitochondrial_genetic_code_id",
      "inherited_MGC_flag",
      "GenBank_hidden_flag",
      "hidden_subtree_root_flag",
      "comments"
    ),
    col_types='i_i_c_c_i_i_i_i_i_i_i_i_c_',
    quote=""
  )

  mssg(verbose, 'building hierarchy table...')
  # will hold a table for every taxonomic level ascending from leaf to root the
  # length of this list will equal the depth of the taxonomic tree (e.g. 37
  # currently)
  hierarchs <- list()
  # set up the base table with columns 'tax_id', 'ancestor', and 'level', where
  # level is 1 for immediate parent
  hierarchs[[1]] <- ncbi_nodes[, c('tax_id', 'parent_tax_id')] %>%
    magrittr::set_names(c('tax_id', 'ancestor'))
  hierarchs[[1]]$level <- 1
  # make a child to parent map
  child2parent <- ncbi_nodes$parent_tax_id
  names(child2parent) <- ncbi_nodes$tax_id
  # Iteratively replace the ancestor column with the ancestor parent until all
  # lineages converge to root. Each iteration is stored in a new table with
  # level incremented.
  while(TRUE) {
    top <- tail(hierarchs, 1)[[1]]
    incomplete <- top$ancestor != 1L # 1 is the taxonomy root id
    top <- top[incomplete, ]
    if(nrow(top) == 0){
      break
    }
    hierarchs[[length(hierarchs)+1]] <- tibble::tibble(
      tax_id = top$tax_id,
      ancestor = child2parent[as.character(top$ancestor)],
      level = rep(length(hierarchs) + 1, nrow(top))
    )
  }
  # Bind all levels into one table.
  hierarchy <- do.call(rbind, hierarchs)
  hierarchy$level <- as.integer(hierarchy$level)


  mssg(verbose, 'building SQLite database...')

  db <- RSQLite::dbConnect(RSQLite::SQLite(), dbname=final_file)

  # Create tables - I have to manually make the `names` table because I have to
  # set `COLLATE NOCASE` at the time of table creation.
  RSQLite::dbExecute(conn=db, "
    CREATE TABLE names (
      tax_id INTEGER,
      name_txt TEXT COLLATE NOCASE,
      unique_name TEXT,
      name_class TEXT
    )
    "
  )

  # Load tables
  RSQLite::dbWriteTable(
    conn   = db,
    name   = 'names',
    value  = as.data.frame(ncbi_names),
    append = TRUE # since I explicitly created the table above
  )
  RSQLite::dbWriteTable(
    conn  = db,
    name  = 'nodes',
    value = as.data.frame(ncbi_nodes),
  )
  RSQLite::dbWriteTable(
    conn  = db,
    name  = 'hierarchy',
    value = as.data.frame(hierarchy),
  )

  # Create indices on tax_id columns
  RSQLite::dbExecute(db,
    'CREATE INDEX tax_id_index_names ON names (tax_id)'
  )
  RSQLite::dbExecute(db,
    'CREATE INDEX name_txt_index_names ON names (name_txt COLLATE NOCASE)'
  )
  RSQLite::dbExecute(db,
    'CREATE INDEX tax_id_index_nodes ON nodes (tax_id)'
  )
  RSQLite::dbExecute(db,
    'CREATE INDEX tax_id_index_hierarchy ON hierarchy (tax_id)'
  )
  RSQLite::dbExecute(db,
    'CREATE INDEX tax_id_ancestor_hierarchy ON hierarchy (ancestor)'
  )

  RSQLite::dbDisconnect(db)

  # cleanup
  mssg(verbose, 'cleaning up...')
  unlink(db_path_file)
  unlink(db_path_dir, recursive = TRUE)

  return(final_file)
}

#' @export
#' @rdname db_download
db_download_itis <- function(verbose = TRUE, overwrite = FALSE, use_curl = TRUE, timeout = 100) {
  # paths
  db_url <- 'https://itis.gov/downloads/itisSqlite.zip'
  db_path <- file.path(tdb_cache$cache_path_get(), 'itisSqlite.zip')
  db_path_file <- file.path(tdb_cache$cache_path_get(), 'itisSqlite')
  final_file <- file.path(tdb_cache$cache_path_get(), 'ITIS.sqlite')

  assert(verbose, "logical")
  assert(overwrite, "logical")
  if (file.exists(final_file) && !overwrite) {
    mssg(verbose, "Database already exists, returning old file")
    return(final_file)
  }
  unlink(final_file, force = TRUE)

  # make home dir if not already present
  tdb_cache$mkdir()
  # download data
  mssg(verbose, 'downloading...')
  if(use_curl) {
    curl::curl_download(db_url, db_path, quiet = TRUE)
  }
  else {
	  options(timeout=timeout)
	  download.file(db_url,db_path)
  }
  # unzip
  mssg(verbose, 'unzipping...')
  utils::unzip(db_path, exdir = db_path_file)
  # get file path
  dirs <- list.dirs(db_path_file, full.names = TRUE)
  dir_date <- dirs[ dirs != db_path_file ]
  sql_path <- list.files(dir_date, pattern = ".sqlite", full.names = TRUE)
  # move database
  file.rename(sql_path, final_file)
  # cleanup
  mssg(verbose, 'cleaning up...')
  unlink(db_path)
  unlink(db_path_file, recursive = TRUE)
  # return path
  return(final_file)
}

#' @export
#' @rdname db_download
db_download_tpl <- function(verbose = TRUE, overwrite = FALSE) {
  db_url <- "https://taxize-dbs.s3-us-west-2.amazonaws.com/plantlist.zip" #nolint
  db_path <- file.path(tdb_cache$cache_path_get(), 'plantlist.zip')
  db_path_file <- file.path(tdb_cache$cache_path_get(), 'plantlist')
  final_file <- file.path(tdb_cache$cache_path_get(), 'plantlist.sqlite')

  assert(verbose, "logical")
  assert(overwrite, "logical")
  if (file.exists(final_file) && !overwrite) {
    mssg(verbose, "Database already exists, returning old file")
    return(final_file)
  }
  unlink(final_file, force = TRUE)

  # make home dir if not already present
  tdb_cache$mkdir()
  # download data
  mssg(verbose, 'downloading...')
  curl::curl_download(db_url, db_path, quiet = TRUE)
  # unzip
  mssg(verbose, 'unzipping...')
  utils::unzip(db_path, exdir = db_path_file)
  # move database
  file.rename(file.path(db_path_file, "plantlist.sqlite"), final_file)
  # cleanup
  mssg(verbose, 'cleaning up...')
  unlink(db_path)
  unlink(db_path_file, recursive = TRUE)
  # return path
  return(final_file)
}

#' @export
#' @rdname db_download
db_download_wfo <- function(verbose = TRUE, overwrite = FALSE) {
  db_url <- "http://104.198.143.165/files/WFO_Backbone/_WFOCompleteBackbone/WFO_Backbone.zip" #nolint
  db_path <- file.path(tdb_cache$cache_path_get(), 'WFO_Backbone.zip')
  db_path_file <- file.path(tdb_cache$cache_path_get(), 'WFO_Backbone')
  class_file <- file.path(tdb_cache$cache_path_get(), 'WFO_Backbone/classification.txt')
  final_file <- file.path(tdb_cache$cache_path_get(), 'wfo.sqlite')

  assert(verbose, "logical")
  assert(overwrite, "logical")
  if (file.exists(final_file) && !overwrite) {
    mssg(verbose, "Database already exists, returning old file")
    return(final_file)
  }
  unlink(final_file, force = TRUE)

  # make home dir if not already present
  tdb_cache$mkdir()
  
  # download data
  mssg(verbose, 'downloading...')
  curl::curl_download(db_url, db_path, quiet = TRUE)
  
  # unzip
  mssg(verbose, 'unzipping...')
  utils::unzip(db_path, exdir = db_path_file)
  
  # create sqlite db
  taxa_txt <- suppressWarnings(readr::read_tsv(class_file, progress = FALSE))
  taxa_txt <- dplyr::rename(taxa_txt, referencez = references)

  mssg(verbose, 'building SQLite database...')
  db <- RSQLite::dbConnect(RSQLite::SQLite(), dbname=final_file)

  # Create table
  RSQLite::dbExecute(conn=db, "
    CREATE TABLE wfo (
      taxonID TEXT,
      scientificNameID TEXT,
      scientificName TEXT,
      taxonRank TEXT,
      parentNameUsageID TEXT,
      scientificNameAuthorship TEXT,
      family TEXT,
      genus TEXT,
      specificEpithet TEXT,
      infraspecificEpithet TEXT,
      verbatimTaxonRank TEXT,
      nomenclaturalStatus TEXT,
      namePublishedIn TEXT,
      taxonomicStatus TEXT,
      acceptedNameUsageID TEXT,
      nameAccordingToID TEXT,
      created TEXT,
      modified TEXT,
      referencez TEXT
    )
    "
  )

  # Load tables
  RSQLite::dbWriteTable(
    conn   = db,
    name   = 'wfo',
    value  = as.data.frame(taxa_txt),
    append = TRUE
  )

  # Create indices on taxonID columns
  RSQLite::dbExecute(db,
    'CREATE INDEX taxonID_index ON wfo (taxonID)'
  )
  RSQLite::dbExecute(db,
    'CREATE INDEX scientificName_index ON wfo (scientificName)'
  )
  RSQLite::dbDisconnect(db)

  # cleanup
  mssg(verbose, 'cleaning up...')
  unlink(db_path)
  unlink(db_path_file, recursive = TRUE)
  # return path
  return(final_file)
}

#' @export
#' @rdname db_download
db_download_col <- function(verbose = TRUE, overwrite = FALSE) {
  db_url <- 'https://taxize-dbs.s3-us-west-2.amazonaws.com/col.zip'
  db_path <- file.path(tdb_cache$cache_path_get(), 'col.sqlite')
  db_path_file <- file.path(tdb_cache$cache_path_get(), 'col.zip')

  assert(verbose, "logical")
  assert(overwrite, "logical")
  if (file.exists(db_path) && !overwrite) {
    mssg(verbose, "Database already exists, returning old file")
    return(db_path)
  }
  unlink(db_path, force = TRUE)

  tdb_cache$mkdir()
  mssg(verbose, 'downloading...')
  curl::curl_download(db_url, db_path_file, quiet = TRUE)

  mssg(verbose, 'unzipping...')
  utils::unzip(db_path_file, exdir = tdb_cache$cache_path_get())

  mssg(verbose, 'cleaning up...')
  unlink(db_path_file)

  mssg(verbose, 'all done...')
  return(db_path)
}

#' @export
#' @rdname db_download
db_download_gbif <- function(verbose = TRUE, overwrite = FALSE) {
  db_url <- 'https://taxize-dbs.s3-us-west-2.amazonaws.com/gbif.zip'
  db_path <- file.path(tdb_cache$cache_path_get(), 'gbif.sqlite')
  db_path_file <- file.path(tdb_cache$cache_path_get(), 'gbif.zip')

  assert(verbose, "logical")
  assert(overwrite, "logical")
  if (file.exists(db_path) && !overwrite) {
    mssg(verbose, "Database already exists, returning old file")
    return(db_path)
  }
  unlink(db_path, force = TRUE)

  tdb_cache$mkdir()
  mssg(verbose, 'downloading...')
  curl::curl_download(db_url, db_path_file, quiet = TRUE)

  mssg(verbose, 'unzipping...')
  utils::unzip(db_path_file, exdir = tdb_cache$cache_path_get())

  mssg(verbose, 'cleaning up...')
  unlink(db_path_file)

  mssg(verbose, 'all done...')
  return(db_path)
}

#' @export
#' @rdname db_download
db_download_wikidata <- function(verbose = TRUE, overwrite = FALSE) {
  db_url <- 'https://zenodo.org/record/1213477/files/wikidata-taxon-info20171227.tsv.gz'

  txt_file <- file.path(tdb_cache$cache_path_get(), 'wikidata-taxon-info20171227.tsv.gz')
  final_file <- file.path(tdb_cache$cache_path_get(), 'wikidata.sqlite')

  if (file.exists(final_file) && !overwrite) {
    mssg(verbose, "Database already exists, returning old file")
    return(final_file)
  }
  unlink(final_file, force = TRUE)

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
