#' Convert taxon IDs to vernacular (common) names
#'
#' @inerhitParams taxid2name
#' @return character vector of vernacular names
#' @export
#' @examples \dontrun{
#' taxid2vernacular(c(3702, 9606, 1106, 1392, 1224, 4444444))
#' }
taxid2vernacular <- function(x, db='ncbi', verbose=TRUE, warn=TRUE, ...){
  result <- ap_vector_dispatch(
    x       = x,
    db      = db,
    cmd     = 'taxid2vernacular',
    verbose = verbose,
    warn    = warn,
    empty   = character(0),
    ...
  )
  if(warn && any(is.na(result))){
    msg <- "No name found for %s of %s taxon IDs"
    msg <- sprintf(msg, sum(is.na(result)), length(result))
    if(verbose){
      msg <- paste0(msg, ". The followings are left unnamed: ", 
        paste0(x[is.na(result)], collapse=', ') 
      )
    }
    warning(msg)
  }
  result 
}

itis_taxid2vernacular <- function(src, x, ...){
  stop("The ITIS database is currently not supported")
}

tpl_taxid2vernacular <- function(src, x, ...){
  stop("The TPL database is currently not supported")
}

col_taxid2vernacular <- function(src, x, ...){
  stop("The COL database is currently not supported")
}

gbif_taxid2vernacular <- function(src, x, ...){
  stop("The GBIF database is currently not supported")
}

ncbi_taxid2vernacular <- function(src, x, ...){
  if(length(x) == 0){
    return(character(0))
  }
  query <- "SELECT tax_id, name_txt FROM names WHERE name_class == 'common name' AND tax_id IN (%s)"
  query <- sprintf(query, sql_integer_list(x))
  tbl <- sql_collect(src, query)
  as.character(tbl$name_txt[match(x, tbl$tax_id)])
}
