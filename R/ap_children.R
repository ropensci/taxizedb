#' Retrieve immediate descendents of a taxon
#'
#' @param x Vector of taxon keys for the given database
#' @param db The database to search
#' @param verbose Print verbose messages
#' @param ... Additional arguments passed to database specific function.
#' @return list of data.frames with the columns: childtaxa_id,  childtaxa_name,
#' childtaxa_rank. This is exactly equivalent to the output of
#' 'taxize::children. 
#' @export
#' @examples
#' \dontrun{
#' children(c(3700, 2))
#' }
children <- function(x, db='ncbi', verbose=TRUE, ...){
  ap_dispatch(x=x, db=db, cmd='children', class='children', verbose=verbose, ...)
}

itis_children <- function(src, x, ...){
  stop("The ITIS database is currently not supported")
}

tpl_children <- function(src, x, ...){
  stop("The TPL database is currently not supported")
}

col_children <- function(src, x, ...){
  stop("The COL database is currently not supported")
}

gbif_children <- function(src, x, ...){
  stop("The GBIF database is currently not supported")
}

ncbi_children <- function(src, x, ...){

  FUN <- function(src, x, ambiguous=FALSE, ...){
    query <- "SELECT tax_id, parent_tax_id, rank FROM nodes where parent_tax_id IN (%s)"
    query <- sprintf(query, paste(paste0("'", x, "'"), collapse=', '))
    children <- sql_collect(src, query)
    # Retrive the names for each ancestral ID
    cmd <- "SELECT tax_id, name_txt FROM names WHERE name_class == 'scientific name' AND tax_id IN (%s)"
    taxid_str <- paste(children$tax_id, collapse=", ")
    children_names <- sql_collect(src, sprintf(cmd, taxid_str))
    # merge names and ids
    merge(children, children_names, by='tax_id') %>%
      dplyr::select(
        parent         = .data$parent_tax_id,
        childtaxa_id   = .data$tax_id,
        childtaxa_name = .data$name_txt,
        childtaxa_rank = .data$rank
      ) %>%
      split(f=factor(.$parent)) %>%
      lapply(function(d){
        d$parent <- NULL
        d$childtaxa_id <- as.character(d$childtaxa_id)
        # convert temporarily to integer for numeric sorting
        d <- d %>%
          dplyr::mutate(childtaxa_id = as.integer(.data$childtaxa_id)) %>%
          dplyr::arrange(-.data$childtaxa_id) %>%
          dplyr::mutate(childtaxa_id = as.character(.data$childtaxa_id))
        if(!ambiguous)
          d <- d[!is_ambiguous(d$childtaxa_name), ]
        rownames(d) <- NULL
        d
      })
  }

  empty_df <- data.frame(
    childtaxa_id   = character(0),
    childtaxa_name = character(0),
    childtaxa_rank = character(0),
    stringsAsFactors=FALSE
  )

  ncbi_apply(src, x, FUN, missing=empty_df, ...)
}
