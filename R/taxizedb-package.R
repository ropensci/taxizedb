#' Taxonomic databases interface
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
#' @examples \dontrun{
#' library(dplyr)
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
#' # data source: catalogue of life
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
#' }
# Needed for use of . in magrittr pipelines
utils::globalVariables(c("."))
NULL
