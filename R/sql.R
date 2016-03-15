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
#' sql_collect(src, "select * from hierarchy limit 5")
#' ## or pipe the src to sql_collect
#' src %>% sql_collect("select * from hierarchy limit 5")
#' }
sql_collect <- function(src, query, ...){
  dplyr::tbl(src, dplyr::sql(query), ...) %>% dplyr::collect()
}
