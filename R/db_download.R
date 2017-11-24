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
#'
#' # NCBI
#' # x <- db_download_ncbi()
#' # db_load_ncbi()
#' # src_ncbi(x)
#' }

#' @export
#' @rdname db_download
db_download_ncbi <- function(verbose = TRUE){
  # set paths
  db_url <- 'ftp://ftp.ncbi.nih.gov/pub/taxonomy/taxdmp.zip'
  db_path_file <- file.path(tdb_cache$cache_path_get(), 'taxdump.zip')
  db_path_dir <- file.path(tdb_cache$cache_path_get(), 'taxdump')
  ncbi_names_file <- file.path(db_path_dir, 'names.dmp')
  ncbi_nodes_file <- file.path(db_path_dir, 'nodes.dmp')
  final_file <- file.path(tdb_cache$cache_path_get(), 'NCBI.sql')

  if(file.exists(final_file)){
    mssg(verbose, "Database already exists, returning old file")
    return(final_file)
  }

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
  # make child to parent id map
  idmap <- as.character(ncbi_nodes$parent_tax_id)
  names(idmap) <- as.character(ncbi_nodes$tax_id)
  # set up root hierarchy table
  hierarchy <- ncbi_nodes[, c('tax_id', 'parent_tax_id')]
  hierarchy$level <- 0
  hierarchy$hierarchy_string <- as.character(hierarchy$tax_id)
  # temporary vector storing whether the hierarchy string has reached root
  complete <- rep(FALSE, nrow(hierarchy))
  # the root taxon ID
  root_node <- 1L
  # each iteration appends the parent id to the hierarchy_string and then replaces gets the new parent
  # the loop breaks when all lineages have been traced to root
  while(any(hierarchy$parent_tax_id != root_node)){
    # add parent to hierarchy string
    hierarchy$hierarchy_string[!complete] <- with(hierarchy[!complete, ], paste(parent_tax_id, hierarchy_string, sep="-")) 
    # bump level
    hierarchy$level[!complete] <- hierarchy$level[!complete] + 1
    # set whether root has been reached
    complete <- hierarchy$parent_tax_id == root_node
    # get new parents
    hierarchy$parent_tax_id <- idmap[as.character(hierarchy$parent_tax_id)] %>% unname
  }
  # remove the self-referential connection
  hierarchy$hierarchy_string[hierarchy$hierarchy_string == "1-1"] <- "1"
  # remove the now unneeded parent column
  hierarchy$parent_tax_id <- NULL
  # make level integrel
  hierarchy$level <- as.integer(hierarchy$level)

  db <- RSQLite::dbConnect(RSQLite::SQLite(), dbname=final_file)
  # Load tables
  RSQLite::dbWriteTable(conn=db, name='names', value=as.data.frame(ncbi_names))
  RSQLite::dbWriteTable(conn=db, name='nodes', value=as.data.frame(ncbi_nodes))
  RSQLite::dbWriteTable(conn=db, name='hierarchy', value=as.data.frame(hierarchy))
  # Create indices on tax_id columns
  RSQLite::dbExecute(conn=db, 'CREATE INDEX tax_id_index_names ON names (tax_id)')
  RSQLite::dbExecute(conn=db, 'CREATE INDEX tax_id_index_nodes ON nodes (tax_id)')
  RSQLite::dbExecute(conn=db, 'CREATE INDEX tax_id_index_hierarchy ON hierarchy (tax_id)')
  RSQLite::dbExecute(conn=db, 'CREATE INDEX name_txt_index_names ON names (name_txt)')
  RSQLite::dbDisconnect(db)

  # cleanup
  mssg(verbose, 'cleaning up...')
  unlink(db_path_file)
  unlink(db_path_dir, recursive = TRUE)

  return(final_file)
}

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
