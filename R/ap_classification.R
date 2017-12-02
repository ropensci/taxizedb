#' Retrieve the taxonomic hierarchies from local database
#'
#' This function is equivalent to the taxize::classification function, except
#' that it uses a local database (so is much faster) and is currently
#' restricted to handling NCBI taxa. The output is identical to
#' taxize::classification.
#'
#' @param x Vector of taxon keys for the given database
#' @param db The database to search
#' @param verbose Print verbose messages
#' @param ... Additional arguments passed to database specific classification functions.
#' @return list of data.frames with the columns: name, rank, and id. This is
#' exactly equivalent to the output of 'taxize::classification'. 
#' @export
#' @examples
#' \dontrun{
#' classification(c(3702, 9606))
#' }
classification <- function(x, db='ncbi', verbose=TRUE, ...){
  ap_dispatch(x=x, db=db, cmd='classification', verbose=verbose, ...)
}

itis_classification <- function(src, x, ...){
  stop("The ITIS database is currently not supported")
}

tpl_classification <- function(src, x, ...){
  stop("The TPL database is currently not supported")
}

col_classification <- function(src, x, ...){
  stop("The COL database is currently not supported")
}

gbif_classification <- function(src, x, ...){
  stop("The GBIF database is currently not supported")
}

ncbi_classification <- function(src, x, ...){

  FUN <- function(src, x, ...){
    # Retrieve the hierarchy for each input taxon id
    cmd <- "SELECT * FROM hierarchy WHERE tax_id IN (%s)"
    query <- sprintf(cmd, paste(x, collapse=", "))
    tbl <- sql_collect(src, query)
    # If no IDs were found, return list of NA
    if(nrow(tbl) == 0){
      lineages <- as.list(as.logical(rep(NA, length(namemap))))
      names(lineages) <- namemap
      return(lineages)
    }
    lineages <-
      tbl %>%
      # Split the hierarchy_string, e.g.  1-123-23-134, into a nested list
      dplyr::mutate(ancestor = strsplit(.data$hierarchy_string, '-')) %>%
      # Unnest ancestors, making one row for each ancestor
      tidyr::unnest(.data$ancestor) %>%
      # Convert strings to integer IDs
      dplyr::mutate(ancestor = as.integer(.data$ancestor)) %>%
      # Add the level, where root == 1
      dplyr::group_by(.data$tax_id) %>%
      dplyr::mutate(level=1:n()) %>%
      dplyr::ungroup() %>%
      # Filter out the fields we need
      dplyr::select(.data$tax_id, .data$level, .data$ancestor)
    # Retrive the names for each ancestral ID
    cmd <- "SELECT tax_id, name_txt FROM names WHERE name_class == 'scientific name' AND tax_id IN (%s)"
    taxid_str <- paste(lineages$ancestor, collapse=", ")
    ancestor_names <- sql_collect(src, sprintf(cmd, taxid_str))
    # Retrive the rank for each ancestral ID
    cmd <- "SELECT tax_id, rank FROM nodes WHERE tax_id IN (%s)"
    ancestor_ranks <- sql_collect(src, sprintf(cmd, taxid_str))
    # Merge in ancestor names and ranks.
    merge(lineages, ancestor_names, by.x='ancestor', by.y='tax_id') %>%
      merge(ancestor_ranks, by.x='ancestor', by.y='tax_id') %>%
      dplyr::arrange(.data$tax_id, .data$level) %>%
      # NOTE: Here I drop the 'level' column. I do this because it is not present
      # in the taxize::classification output. However, without the level column,
      # the ancestor order is encoded only in the row order of the data.frame,
      # which is not robost.
      dplyr::select(
        tax_id = .data$tax_id,
        name   = .data$name_txt,
        rank   = .data$rank,
        id     = .data$ancestor
      ) %>%
      # Split the data.frame by input taxon ID
      split(factor(.$tax_id)) %>%
      lapply(function(x) {
          x$tax_id = NULL
          # NOTE: Remove the root node, for consistency with 'taxize'. The root
          # node really is important, though, because viruses are a thing.
          x <- x[x$name != 'root', ]
          # To match the type in 'taxize':
          x$id <- as.character(x$id)
          rownames(x) <- NULL
          x
      })
  }

  ncbi_apply(src, x, FUN, ...)
}
