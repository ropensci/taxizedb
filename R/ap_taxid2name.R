#' Convert taxon IDs to scientific names
#'
#' @param x Vector of taxon keys for the given database
#' @param db The database to search
#' @param verbose Print verbose messages
#' @param ... Additional arguments passed to database specific classification functions.
#' @return character vector of scientific names
#' @export
#' @examples
#' \dontrun{
#' taxid2name(c(3702, 9606))
#' }
taxid2name <- function(x, db='ncbi', verbose=TRUE, ...){
  ap_vector_dispatch(x=x, db=db, cmd='taxid2name', verbose=verbose, ...)
}

itis_name2taxid <- function(src, x, ...){
  stop("The ITIS database is currently not supported")
}

tpl_name2taxid <- function(src, x, ...){
  stop("The TPL database is currently not supported")
}

col_name2taxid <- function(src, x, ...){
  stop("The COL database is currently not supported")
}

gbif_name2taxid <- function(src, x, ...){
  stop("The GBIF database is currently not supported")
}

ncbi_taxid2name <- function(src, x, ...){
  if(length(x) == 0){
    return(character(0))
  }
  query <- "SELECT tax_id, name_txt FROM names WHERE name_class == 'scientific name' AND tax_id IN (%s)"
  query <- sprintf(query, paste(x, collapse=", "))
  tbl <- sql_collect(src, query)
  as.character(tbl$name_txt[match(x, tbl$tax_id)])
}
