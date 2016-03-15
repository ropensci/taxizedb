#' Query and get data back into a data.frame
#'
#' @export
#' @param src An \code{src} object, result of calling \code{\link{src_itis}},
#' \code{\link{src_col}}, or \code{\link{src_tpl}}
#' @param query A SQL query
#' @param ... further args passed on to \code{\link[dplyr]{tbl}}
#' @details we run \code{\link[dplyr]{tbl}}, then \code{\link[dplyr]{collect}}
#' @examples \dontrun{
#' src <- src_itis()
#'
#' # use SQL syntax
#' #sql_collect(src, "<some query>")
#' sql_collect(src, "select * from hierarchy limit 5")
#' ## or pipe the src to sql_collect
#' src %>% sql_collect("select * from hierarchy limit 5")
#'
#' # use dplyr verbs
#' src %>%
#'   tbl("hierarchy") %>%
#'   top_n(10)
#'
#' ## or create tbl object for repeated use
#' hiers <- src %>% tbl("hierarchy")
#' hiers %>% top_n(10)
#' hiers %>% select(tsn, level)
#'
#' # theplantlist
#' src <- src_tpl()
#' tpl <- src %>% tbl("plantlist")
#' tpl %>% filter(family == "Pinaceae")
#'
#' # catalogue of life
#' src <- src_col()
#' }
sql_collect <- function(src, query, ...){
  dplyr::tbl(src, dplyr::sql(query), ...) %>% dplyr::collect()
}
