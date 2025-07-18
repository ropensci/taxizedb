test_that("We can actually download, load, and query all databases", {

  testthat::skip_if_not(Sys.getenv("taxizedb_docker") == "test" )

  ## These should load from cache if we already have the download!
  gbif <- db_download_gbif()
  itis <- db_download_itis()
  col <- db_download_col()
  ncbi <- db_download_ncbi()
  wfo <- db_download_wfo()
  wikidata <- db_download_wikidata()

  # gbif <- "~/.cache/R/taxizedb/gbif.sqlite"      # SQLITE
  # itis <- "~/.cache/R/taxizedb/ITIS.sqlite"      # SQLITE
  # tpl  <- "~/.cache/R/taxizedb/plantlist.sqlite" # SQLITE
  # col <-  "~/.cache/R/taxizedb/col.sqlite"       # SQLITE
  # ncbi <- "~/.cache/R/taxizedb/NCBI.sql"         # SQLITE?
  # wfo <-  "~/.cache/R/taxizedb/wfo.sqlite"       # SQLITE

  ## Load the actual data:
  gbif_db <- src_gbif()
  itis_db <- src_itis()
  col_db <- src_col()
  ncbi_db <- src_ncbi()
  wfo_db <- src_wfo()
  wikitaxa_db <- src_wikidata()

  # TPL is no longer accessible. We can only test locally.
  file <- file.path(tdb_cache$cache_path_get(), 'plantlist.sqlite')
  if (file.exists(file)) {
    tpl <- db_download_tpl()
    tpl_db <- src_tpl()
  }
})
