skip_on_cran()

library(dplyr)
library(dbplyr)
library(taxizedb)

test_that("We can actually download, load, and query all databases", {

  testthat::skip_if_not(Sys.getenv("taxizedb_docker") == "test" )

  ## These should load from cache if we already have the download!
  gbif <- db_download_gbif()
  itis <- db_download_itis()
  tpl <- db_download_tpl()
  col <- db_download_col()
  ncbi <- db_download_ncbi()

  gbif <- "~/.cache/R/taxizedb/gbif.sqlite"
  itis <- "~/.cache/R/taxizedb/ITIS.sql"      #POSTGRES
  tpl  <- "~/.cache/R/taxizedb/plantlist.sql" # POSTGRES
  col <-  "~/.cache/R/taxizedb/col.sql" # MySQL
  ncbi <- "~/.cache/R/taxizedb/NCBI.sql" # SQLITE?

  ## NOTE FIXME argument should be called password, not pwd, to be consistent with src_ and DBI

  ## Load the actual data:

  col_db <- src_col(host="mariadb", user="root", password="password")
  tpl_db <- src_tpl(user = "postgres", password = "password", host = "postgres")

  gbif_db <- src_gbif(gbif)
  ncbi_db <- src_ncbi(ncbi)
})
