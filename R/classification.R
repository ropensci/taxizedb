#' Retrieve the taxonomic hierarchies from local database
#'
#' This function is equivalent to the taxize::classification function, except
#' that it uses a local database (so is much faster) and is currently
#' restricted to handling NCBI taxa. The output is identical to
#' taxize::classification.
#'
#' @param src src_dbi database link to a local database 
#' @param taxa Vector of taxonomy ids
#' @param db The database to search (currently only NCBI is implemented)
#' @return list of data.frames with the columns: name, rank, and id. This is
#' exactly equivalent to the output of 'taxize::classification'. 
#' @export
classification <- function(src, taxa, db='ncbi'){

  if(db != 'ncbi'){
    stop("Sorry, only the NCBI database is currently supported")
  }

  # Retrieve the hierarchy for each input taxon id
  cmd <- "SELECT * FROM hierarchy WHERE tax_id IN (%s)"
  taxid_str <- paste(taxa, collapse=", ")
  lineages <- sql_collect(src, sprintf(cmd, taxid_str)) %>%
    # Split the hierarchy_string, e.g.  1-123-23-134, into a nested list
    dplyr::mutate(ancestor = strsplit(hierarchy_string, '-')) %>%
    # Unnest ancestors, making one row for each ancestor
    tidyr::unnest(ancestor) %>%
    # Convert strings to integer IDs
    dplyr::mutate(ancestor = as.integer(ancestor)) %>%
    # Add the level, where root == 1
    dplyr::group_by(tax_id) %>%
    dplyr::mutate(level=1:n()) %>%
    dplyr::ungroup() %>%
    # Filter out the fields we need
    dplyr::select(tax_id, level, ancestor)

  # Retrive the names for each ancestral ID
  cmd <- "SELECT tax_id, name_txt FROM names WHERE name_class == 'scientific name' AND tax_id IN (%s)"
  taxid_str <- paste(lineages$ancestor, collapse=", ")
  ancestor_names <- sql_collect(src, sprintf(cmd, taxid_str))

  # Retrive the rank for each ancestral ID
  cmd <- "SELECT tax_id, rank FROM nodes WHERE tax_id IN (%s)"
  ancestor_ranks <- sql_collect(src, sprintf(cmd, taxid_str))

  # Merge in ancestor names and ranks.
  lineages <- merge(lineages, ancestor_names, by.x='ancestor', by.y='tax_id') %>%
    merge(ancestor_ranks, by.x='ancestor', by.y='tax_id') %>%
    dplyr::arrange(tax_id, level) %>%
    # NOTE: Here I drop the 'level' column. I do this because it is not present
    # in the taxize::classification output. However, without the level column,
    # the ancestor order is encoded only in the row order of the data.frame,
    # which is not robost.
    dplyr::select(tax_id, name=name_txt, rank, id=ancestor) %>%
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

  attributes(lineages) <- list(names=names(lineages), class='classification', db=db)

  lineages
}
