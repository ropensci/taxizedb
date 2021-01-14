#' Convert taxon IDs to scientific names
#'
#' @export
#' @param x (character) Vector of taxon keys for the given database
#' @param db (character) The database to search, one of ncbi, itis, gbif,
#' col, wfo, or tpl
#' @param verbose (logical) Print verbose messages
#' @param warn (logical) If `TRUE`, raise a warning if any taxon IDs can not
#' be found
#' @param ... Additional arguments passed to database specific classification
#' functions
#' @return character vector of scientific names
#' @examples \dontrun{
#' taxid2name(c(3702, 9606))
#' taxid2name(c(154395, 154357, 23041, 154396), db = "itis")
#' taxid2name(c('wfo-0000541830', 'wfo-0000291463'), db = "wfo")
#' taxid2name("wfo-7000000057", db="wfo")
#' taxid2name(2877951, db="gbif")
#' taxid2name(c(2877951, 5386), db="gbif")
#' taxid2name(c(3960765, 3953606, 3953010), db="col")
#' taxid2name(c("kew-2614538", "kew-2895433", "kew-2615007"), db="tpl")
#' }
taxid2name <- function(x, db='ncbi', verbose=TRUE, warn=TRUE, ...){
  result <- ap_vector_dispatch(
    x       = x,
    db      = db,
    cmd     = 'taxid2name',
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

itis_taxid2name <- function(src, x, ...){
  if (length(x) == 0) return(character(0))
  query <- "SELECT tsn,complete_name FROM taxonomic_units WHERE tsn IN ('%s')"
  query <- sprintf(query, paste0(x, collapse = "','"))
  tbl <- sql_collect(src, query)
  tbl$complete_name[match(x, tbl$tsn)]
}

wfo_taxid2name <- function(src, x, ...){
  if (length(x) == 0) return(character(0))
  query <- "SELECT taxonID,scientificName FROM wfo WHERE taxonID IN ('%s')"
  query <- sprintf(query, paste0(x, collapse = "','"))
  tbl <- sql_collect(src, query)
  tbl$scientificName[match(x, tbl$taxonID)]
}

tpl_taxid2name <- function(src, x, ...){
  if (length(x) == 0) return(character(0))
  query <- "SELECT id,scientificname FROM tpl WHERE id IN ('%s')"
  query <- sprintf(query, paste0(x, collapse = "','"))
  tbl <- sql_collect(src, query)
  tbl$scientificname[match(x, tbl$id)]
}

col_taxid2name <- function(src, x, ...){
  if (length(x) == 0) return(character(0))
  query <- "SELECT taxonID,scientificName FROM taxa WHERE taxonID IN ('%s')"
  query <- sprintf(query, paste0(x, collapse = "','"))
  tbl <- sql_collect(src, query)
  tbl$scientificName[match(x, tbl$taxonID)]
}

gbif_taxid2name <- function(src, x, ...){
  if (length(x) == 0) return(character(0))
  query <- "SELECT taxonID,canonicalName FROM gbif WHERE taxonID IN ('%s')"
  query <- sprintf(query, paste0(x, collapse = "','"))
  tbl <- sql_collect(src, query)
  tbl$canonicalName[match(x, tbl$taxonID)]
}

ncbi_taxid2name <- function(src, x, ...){
  if (length(x) == 0) return(character(0))
  query <- "SELECT tax_id, name_txt FROM names WHERE name_class == 'scientific name' AND tax_id IN (%s)"
  query <- sprintf(query, sql_integer_list(x))
  tbl <- sql_collect(src, query)
  as.character(tbl$name_txt[match(x, tbl$tax_id)])
}
