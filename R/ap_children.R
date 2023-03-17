#' Retrieve immediate descendents of a taxon
#'
#' @export
#' @param x (character) Vector of taxon keys for the given database
#' @param db (character) The database to search, one of ncbi, itis, gbif,
#' col, or wfo
#' @param verbose (logical) Print verbose messages
#' @param ... Additional arguments passed to database specific function.
#' @return list of tibbles with the columns: id, name, rank. This is exactly
#' equivalent to the output of `taxize::children()`
#' @examples \dontrun{
#' children(c(3700, 2))
#' children(c(154395, 154357), db="itis")
#' children("wfo-4000032377", db="wfo")
#' children(2877951, db="gbif")
#' children(3960765, db="col") # Abies
#' }
children <- function(x, db='ncbi', verbose=TRUE, ...){
  ap_dispatch(x=x, db=db, cmd='children', class='children', verbose=verbose, ...)
}

itis_children <- function(src, x, ...){
  FUN <- function(x, src, ...) {
    ranks <- unique(sql_collect(src, 'select * from taxon_unit_types'))
    children <- 
      sql_collect(src, sprintf("select * from hierarchy where Parent_TSN = '%s'", x))
    tsns <- children$TSN
    child_query <- sprintf(
      "SELECT tsn,rank_id,complete_name FROM taxonomic_units WHERE tsn IN ('%s')", 
      paste0(tsns, collapse = "','"))
    child_df <- sql_collect(src, child_query)
    tmp <- unique(dplyr::left_join(
      child_df, 
      dplyr::select(ranks, rank_id, rank_name),
      by = "rank_id", multiple = "all"))
    tmp$rank_name <- tolower(tmp$rank_name)
    # tmp$rank_id <- NULL
    tmp <- dplyr::rename(tmp, id = 'tsn', name = 'complete_name', rank = 'rank_name')
    return(tmp)
  }
  stats::setNames(lapply(x, FUN, src = src), x)
}

wfo_children <- function(src, x, ...){
  FUN <- function(x, src, ...) {
    cols <- "taxonID,taxonRank,scientificName,parentNameUsageID"
    z <- sql_collect(src,
      sprintf("select %s from wfo where parentNameUsageID = '%s'", cols, x))
    z$taxonRank <- tolower(z$taxonRank)
    z$parentNameUsageID <- NULL
    z <- dplyr::rename(z, id = 'taxonID', rank = 'taxonRank', name = 'scientificName')
    dplyr::select(z, id, name, rank)
  }
  stats::setNames(lapply(x, FUN, src = src), x)
}

tpl_children <- function(src, x, ...){
  stop("The TPL database is not supported")
}

col_children <- function(src, x, ...){
  FUN <- function(x, src, ...) {
    cols <- "taxonID,taxonRank,scientificName,parentNameUsageID"
    z <- sql_collect(src,
      sprintf("select %s from taxa where parentNameUsageID = '%s'", cols, x))
    z$taxonRank <- tolower(z$taxonRank)
    z$parentNameUsageID <- NULL
    z <- dplyr::rename(z, id = 'taxonID', rank = 'taxonRank', name = 'scientificName')
    dplyr::select(z, id, name, rank)
  }
  stats::setNames(lapply(x, FUN, src = src), x)
}

gbif_children <- function(src, x, ...){
  FUN <- function(x, src, ...) {
    cols <- "taxonID,taxonRank,scientificName,parentNameUsageID"
    z <- sql_collect(src,
      sprintf("select %s from gbif where parentNameUsageID = '%s'", cols, x))
    z$taxonRank <- tolower(z$taxonRank)
    z$parentNameUsageID <- NULL
    z <- dplyr::rename(z, id = 'taxonID', rank = 'taxonRank', name = 'scientificName')
    dplyr::select(z, id, name, rank)
  }
  stats::setNames(lapply(x, FUN, src = src), x)
}

ncbi_children <- function(src, x, ...){

  FUN <- function(src, x, ambiguous=FALSE, ...){
    cmd <- "SELECT tax_id, parent_tax_id, rank FROM nodes where parent_tax_id IN (%s)"
    query <- sprintf(cmd, sql_character_list(x))
    children <- sql_collect(src, query)
    # Retrive the names for each ancestral ID
    cmd <- "SELECT tax_id, name_txt FROM names WHERE name_class == 'scientific name' AND tax_id IN (%s)"
    query <- sprintf(cmd, sql_integer_list(children$tax_id))
    children_names <- sql_collect(src, query)
    # merge names and ids
    merge(children, children_names, by='tax_id') %>%
      dplyr::select(
        parent         = .data$parent_tax_id,
        id   = .data$tax_id,
        name = .data$name_txt,
        rank = .data$rank
      ) %>%
      split(f=factor(.$parent)) %>%
      lapply(function(d){
        d$parent <- NULL
        d$id <- as.character(d$id)
        # convert temporarily to integer for numeric sorting
        d <- d %>%
          dplyr::mutate(id = as.integer(.data$id)) %>%
          dplyr::arrange(-.data$id) %>%
          dplyr::mutate(id = as.character(.data$id))
        if(!ambiguous)
          d <- d[!is_ambiguous(d$name), ]
        rownames(d) <- NULL
        dplyr::as_tibble(d)
      })
  }

  empty_df <- data.frame(
    id   = character(0),
    name = character(0),
    rank = character(0),
    stringsAsFactors=FALSE
  )

  ncbi_apply(src, x, FUN, missing=empty_df, ...)
}
