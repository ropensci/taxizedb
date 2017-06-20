#' Taxonomic databases interface
#'
#' @importFrom DBI dbConnect dbDisconnect dbSendQuery
#' @importFrom RPostgreSQL PostgreSQL
#' @importFrom RMySQL MySQL
#' @importFrom RSQLite SQLite
#' @importFrom dplyr tbl sql collect
#' @importFrom dbplyr src_dbi
#' @importFrom curl curl_download
#' @importFrom magrittr %>%
#' @name taxizedb-package
#' @aliases taxizedb
#' @docType package
#' @author Scott Chamberlain \email{myrmecocystus@@gmail.com}
#' @keywords package
#'
#' @section Database setup/user/pwd:
#' Every user will have a unique combination of username, password, and
#' platform, so we can't make the functions in this package work out of the
#' box in every situation.
#'
#' @examples \dontrun{
#' # IMPORTANT: Remember to start your PostgreSQL database for ITIS
#' # and ThePlantList and your MySQL database for COL
#'
#' # data source: ITIS
#' ## download ITIS database
#' x <- db_download_itis()
#' db_load_itis(x)
#'
#' ## connect to the ITIS database
#' src <- src_itis()
#'
#' ## use SQL syntax
#' sql_collect(src, "select * from hierarchy limit 5")
#' ### or pipe the src to sql_collect
#' src %>% sql_collect("select * from hierarchy limit 5")
#'
#' ## use dplyr verbs
#' src %>%
#'   tbl("hierarchy") %>%
#'   top_n(10)
#'
#' ## or create tbl object for repeated use
#' hiers <- src %>% tbl("hierarchy")
#' hiers %>% top_n(10)
#' hiers %>% select(tsn, level)
#'
#'
#' # data source: theplantlist
#' ## download tpl datababase
#' x <- db_download_tpl()
#' db_load_tpl(x)
#'
#' ## connecto the tpl database
#' src <- src_tpl()
#'
#' ## do queries
#' tpl <- src %>% tbl("plantlist")
#' tpl %>% filter(family == "Pinaceae")
#'
#' # data source: catalogue of life
#' ## download col datababase
#' x <- db_download_col()
#' db_load_col(x)
#'
#' ## connecto the col database
#' src <- src_col()
#'
#' ## do queries
#' cnames <- src %>% tbl("common_name_element")
#' cnames %>% select(name)
#' }
NULL
