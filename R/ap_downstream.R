#' Retrieve all taxa descending from a vector of taxa
#'
#' This function is nearly equivalent to the taxize::downstream function.
#'
#' @param x Vector of taxon keys for the given database
#' @param db The database to search
#' @param verbose Print verbose messages
#' @param ... Additional arguments passed to database specific downstream functions.
#' @return list of data.frames with the columns: childtaxa_id, childtaxa_name,
#' and rank. This is exactly equivalent to the output of 'taxize::downstream'. 
#' @export
#' @examples
#' # get descendents from all ranks
#' downstream(c(3700, 9605))
#'
#' # limit results to species
#' downstream(c(3700, 9605), downto='species')
#'
#' # allow ambiguous nodes but no ambiguous species
#' downstream(
#'   c(3700, 9605),
#'   downto='species',
#'   ambiguous_nodes=FALSE,
#'   ambiguous_species=TRUE
#' )
downstream <- function(x, db='ncbi', verbose=TRUE, ...){
  ap_dispatch(
    x       = x,
    db      = db,
    cmd     = 'downstream',
    ...
  )
}

itis_downstream <- function(src, x, ...){
  stop("The ITIS database is currently not supported")
}

tpl_downstream <- function(src, x, ...){
  stop("The TPL database is currently not supported")
}

col_downstream <- function(src, x, ...){
  stop("The COL database is currently not supported")
}

gbif_downstream <- function(src, x, ...){
  stop("The GBIF database is currently not supported")
}

ncbi_downstream <- function(src, x, ...){

  FUN <- function(
    src,
    x,
    ambiguous_nodes   = FALSE,
    ambiguous_species = FALSE,
    downto            = NULL,
    ...
  ){
    cmd <- "SELECT tax_id, level, ancestor FROM hierarchy WHERE ancestor IN (%s)"
    query <- sprintf(cmd, paste(x, collapse=", "))
    sql_collect(src, query) %>%
      dplyr::mutate(
        rank = taxid2rank(.data$tax_id),
        childtaxa_name = taxid2name(.data$tax_id)
      ) %>%
      dplyr::select(
        childtaxa_id   = .data$tax_id,
        childtaxa_name = .data$childtaxa_name,
        rank           = .data$rank,
        key            = .data$ancestor
      ) %>%
      {
        if(!is.null(downto)){
          dplyr::filter(., .data$rank == downto)
        } else {
          .
        }
      } %>%
      dplyr::filter(ambiguous_nodes   | (.$rank == 'species' | !is_ambiguous(.data$childtaxa_name))) %>%
      dplyr::filter(ambiguous_species | (.$rank != 'species' | !is_ambiguous(.data$childtaxa_name))) %>%
      split(f=.$key) %>%
      lapply(function(d){
        d$key <- NULL
        d <-  dplyr::arrange(as.data.frame(d), -.data$childtaxa_id)
        d$childtaxa_id <- as.character(d$childtaxa_id)
        d
      })
  }

  ncbi_apply(src, x, FUN, ...)
}
