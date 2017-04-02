#' Query and get data back into a data.frame
#'
#' @export
#' @param src An `src` object, result of calling [src_itis()],
#' [src_col()], or [src_tpl()]
#' @param query A SQL query
#' @param ... further args passed on to [dplyr::tbl()]
#' @details we run [dplyr::tbl()], then [dplyr::collect()]
#' @examples \dontrun{
#' src <- src_itis()
#' sql_collect(src, "select * from hierarchy limit 5")
#' ## or pipe the src to sql_collect
#' src %>% sql_collect("select * from hierarchy limit 5")
#' }
sql_collect <- function(src, query, ...){
  dplyr::tbl(src, dplyr::sql(query), ...) %>% dplyr::collect()
}
