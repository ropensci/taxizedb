library(dplyr)
library(dbplyr)
library(taxizedb)

test_that("We can actually download, load, and query all databases", {

  testthat::skip_on_travis()
  testthat::skip_on_cran()
  testthat::skip_on_appveyor()

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

## Working:
db_load_col(col, host="mariadb", user="root", pwd="password")
db_load_tpl(tpl, user = "postgres", pwd = "password", host = "postgres")

## not needed:
db_load_ncbi()
db_load_gbif()

## Need to fix locale issue
db_load_itis(itis, user = "postgres", pwd = "password", host = "postgres")


## Load the actual data:

col_db <- src_col(host="mariadb", user="root", password="password")
tpl_db <- src_tpl(user = "postgres", password = "password", host = "postgres")

gbif_db <- src_gbif(gbif)
ncbi_db <- src_ncbi(ncbi)

#itis_db <- src_itis(user = "postgres", password = "password", host = "postgres")

})
