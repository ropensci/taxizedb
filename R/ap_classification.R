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
  ap_dispatch(
    x       = x,
    db      = db,
    cmd     = 'classification',
    verbose = verbose,
    ...
  )
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
    cmd <- "SELECT tax_id, level, ancestor FROM hierarchy WHERE tax_id IN (%s)"
    query <- sprintf(cmd, sql_integer_list(x))
    tbl <- sql_collect(src, query)
    # If no IDs were found, return list of NA
    if(nrow(tbl) == 0){
      lineages <- as.list(as.logical(rep(NA, length(x))))
      names(lineages) <- x
      return(lineages)
    }

    # Add the query to the lineage as the lowest level
    rbind(tbl, tibble::tibble(
      tax_id   = x,
      ancestor = x,
      level    = rep(0L, length(x))
    )) %>%
    # NOTE: Remove the root node, for consistency with 'taxize'. The root
    # node really is important, though, because viruses are a thing.
    dplyr::filter(.data$ancestor != 1L) %>%
    # Add ranks (TODO: add taxid2rank function)
    merge({
      cmd <- "SELECT tax_id, rank FROM nodes WHERE tax_id IN (%s)"
      query <- sprintf(cmd, sql_integer_list(.$ancestor))
      sql_collect(src, query)
    }, by.x='ancestor', by.y='tax_id') %>%
    dplyr::mutate(
      # make taxon IDs character vectors (for consistency with taxize)
      ancestor = as.character(.data$ancestor),
      # add ancestor scientific name
      name = taxid2name(.data$ancestor)
    ) %>%
    split(f=.$tax_id) %>%
    lapply(function(d)
      dplyr::arrange(d, -.data$level) %>%
      # NOTE: Here I drop the 'level' column. I do this because it is not present
      # in the taxize::classification output. However, without the level column,
      # the ancestor order is encoded only in the row order of the data.frame,
      # which is not robost.
      dplyr::select(
        name = .data$name,
        rank = .data$rank,
        id   = .data$ancestor
      )
    )
  }

  ncbi_apply(src, x, FUN, ...)
}
