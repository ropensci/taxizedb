#' @title taxizedb
#' @description Taxonomic databases interface
#'
#' @importFrom DBI dbConnect dbDisconnect dbSendQuery
#' @importFrom RSQLite SQLite
#' @importFrom dplyr tbl sql collect n
#' @importFrom dbplyr src_dbi
#' @importFrom rlang .data
#' @importFrom utils tail
#' @importFrom curl curl_download
#' @importFrom magrittr %>%
#' @name taxizedb-package
#' @aliases taxizedb
#' @docType package
#' @keywords package
#' 
#' @section Supported data sources and database structure:
#' All are using SQLite as the database
#'
#' - NCBI: text files are provided by NCBI, which we stitch into a sqlite db
#' - ITIS: they provide a sqlite dump, which we use here
#' - The PlantList: created from stitching together csv files. this
#' source is no longer updated as far as we can tell. they say they've
#' moved focus to the World Flora Online
#' - Catalogue of Life: created from Darwin Core Archive dump. Using the
#' latest monthly edition via 
#' http://www.catalogueoflife.org/DCA_Export/archive.php
#' - GBIF: created from Darwin Core Archive dump. right now we only have
#' the taxonomy table (called gbif), but will add the other tables in the
#' darwin core archive later
#' - Wikidata: aggregated taxonomy of Open Tree of Life, GLoBI and Wikidata. 
#' On Zenodo, created by Joritt Poelen of GLOBI.
#' - World Flora Online: http://www.worldfloraonline.org/
#'
#' @section Update schedule for databases:
#'
#' - NCBI: since `db_download_ncbi` creates the database when the function
#' is called, it's updated whenever you run the function
#' - ITIS: since ITIS provides the sqlite database as a download, you can
#' delete the old file and run `db_download_itis` to get a new dump;
#' they I think update the dumps every month or so
#' - The PlantList: no longer updated, so you shouldn't need to download
#' this after the first download
#' - Catalogue of Life: a GitHub Actions job runs once a day at 00:00 UTC,
#' building the lastest COL data into a SQLite database thats hosted on
#' Amazon S3
#' - GBIF: a GitHub Actions job runs once a day at 00:00 UTC,
#' building the lastest COL data into a SQLite database thats hosted on
#' Amazon S3
#' - Wikidata: last updated April 6, 2018. Scripts are available to 
#' update the data if you prefer to do it yourself.
#' - World Flora Online: since `db_download_wfo` creates the database when
#' the function is called, it's updated whenever you run the function
#'
#' @section Links:
#'
#' - NCBI: ftp://ftp.ncbi.nih.gov/pub/taxonomy/
#' - ITIS: https://www.itis.gov/downloads/index.html
#' - The PlantList - http://www.theplantlist.org/
#' - Catalogue of Life:
#'   via http://www.catalogueoflife.org/content/annual-checklist-archive
#' - GBIF: http://rs.gbif.org/datasets/backbone/
#' - Wikidata: https://zenodo.org/record/1213477
#' - World Flora Online: http://www.worldfloraonline.org/
#'
#' @examples \dontrun{
#' library(dplyr)
#' 
#' # data source: NCBI
#' db_download_ncbi()
#' src <- src_ncbi()
#' df <- tbl(src, "names")
#' filter(df, name_class == "scientific name")
#' 
#' # data source: ITIS
#' ## download ITIS database
#' db_download_itis()
#' ## connect to the ITIS database
#' src <- src_itis()
#' ## use SQL syntax
#' sql_collect(src, "select * from hierarchy limit 5")
#' ### or pipe the src to sql_collect
#' src %>% sql_collect("select * from hierarchy limit 5")
#' ## use dplyr verbs
#' src %>%
#'   tbl("hierarchy") %>%
#'   filter(ChildrenCount > 1000)
#' ## or create tbl object for repeated use
#' hiers <- src %>% tbl("hierarchy")
#' hiers %>% select(TSN, level)
#'
#' # data source: The PlantList
#' ## download tpl datababase
#' db_download_tpl()
#' ## connecto the tpl database
#' src <- src_tpl()
#' ## do queries
#' tpl <- tbl(src, "tpl")
#' filter(tpl, Family == "Pinaceae")
#'
#' # data source: Catalogue of Life
#' ## download col datababase
#' db_download_col()
#' ## connec to the col database
#' src <- src_col()
#' ## do queries
#' names <- tbl(src, "taxa")
#' select(names, taxonID, scientificName)
#' 
#' # data source: GBIF
#' ## download gbif datababase
#' db_download_gbif()
#' ## connecto the gbif database
#' src <- src_gbif()
#' ## do queries
#' df <- tbl(src, "gbif")
#' select(df, taxonID, scientificName)
#' 
#' # data source: Wikidata
#' db_download_wikidata()
#' src <- src_wikidata()
#' df <- tbl(src, "wikidata")
#' filter(df, rank_id == "Q7432")
#' 
#' # data source: World Flora Online
#' db_download_wfo()
#' src <- src_wfo()
#' df <- tbl(src, "wfo")
#' filter(df, taxonID == "wfo-0000000010")
#' }
# Needed for use of . in magrittr pipelines
utils::globalVariables(c(".", "rank_id", "rank_name", "kingdom_id",
  "name", "id", "references", "desc"))
NULL
