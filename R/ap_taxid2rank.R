#' Convert taxon IDs to scientific ranks
#'
#' @export
#' @param x (character) Vector of taxon keys (name or id) for the given
#' database
#' @param db (character) The database to search, one of ncbi or itis
#' @param verbose (logical) Print verbose messages
#' @param warn (logical) If `TRUE`, raise a warning if any taxon IDs can not
#' be found
#' @param ... Additional arguments passed to database specific classification
#' functions
#' @return character vector of ranks in the same order as the inputs
#' @examples \dontrun{
#' taxid2rank(c(3701, 9606))
#' taxid2rank(c(154395, 154357, 23041, 154396), db="itis")
#' }
taxid2rank <- function(x, db='ncbi', verbose=TRUE, warn=TRUE, ...){
  result <- ap_vector_dispatch(
    x       = x,
    db      = db,
    cmd     = 'taxid2rank',
    verbose = verbose,
    warn    = warn,
    empty   = character(0),
    ...
  )
  if(warn && any(is.na(result))){
    msg <- "No rank found for %s of %s taxon IDs"
    msg <- sprintf(msg, sum(is.na(result)), length(result))
    if(verbose){
      msg <- paste0(msg, ". The followings are left unrankd: ",
        paste0(x[is.na(result)], collapse=', ')
      )
    }
    warning(msg)
  }
  result
}

itis_taxid2rank <- function(src, x, ...){
  if (length(x) == 0) return(character(0))
  ranks <- unique(sql_collect(src, 'select * from taxon_unit_types'))
  query <- "SELECT tsn,rank_id FROM taxonomic_units WHERE tsn IN ('%s')"
  query <- sprintf(query, paste0(x, collapse = "','"))
  tbl <- sql_collect(src, query)
  z <- dplyr::left_join(tbl,
    unique(dplyr::select(ranks, rank_id, rank_name)),
    by = "rank_id")
  tolower(z$rank_name[match(x, z$tsn)])
}

tpl_taxid2rank <- function(src, x, ...){
  stop("The TPL database is currently not supported")
}

col_taxid2rank <- function(src, x, ...){
  stop("The COL database is currently not supported")
}

gbif_taxid2rank <- function(src, x, ...){
  stop("The GBIF database is currently not supported")
}

ncbi_taxid2rank <- function(src, x, ...){
  if (length(x) == 0) return(character(0))
  query <- "SELECT tax_id, rank FROM nodes WHERE tax_id IN (%s)"
  query <- sprintf(query, sql_integer_list(x))
  tbl <- sql_collect(src, query)
  as.character(tbl$rank[match(x, tbl$tax_id)])
}
