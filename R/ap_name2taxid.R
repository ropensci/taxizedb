#' Convert species names to taxon IDs
#'
#' \code{name2taxid} returns a vector and dies if there are any ambiguous
#' names. \code{name2taxid_map} returns a data.frame mapping names to ids.
#'
#' @param x Vector of taxon keys for the given database
#' @param db The database to search
#' @param verbose Print verbose messages
#' @param out_type character "uid" for an ID vector, "summary" for a table with
#'        columns 'tax_id' and 'tax_name'.
#' @param ... Additional arguments passed to database specific classification functions.
#' @export
#' @examples
#' \dontrun{
#' name2taxid(c('Arabidopsis thaliana', 'pig'))
#' }
name2taxid <- function(x, db='ncbi', verbose=TRUE, out_type=c("uid", "summary"), ...){
  result <- ap_vector_dispatch(
    x       = x,
    db      = db,
    cmd     = 'name2taxid',
    verbose = verbose,
    empty   = tibble::data_frame(name_txt=character(), tax_id=character()),
    ...
  )
  if(identical(out_type[1], "summary")){
    result
  } else if (identical(out_type[1], "uid")) {
    ids <- result$tax_id
    if(any(duplicated(result$name_txt))){
      stop("Some of the input names are ambiguous, try setting out_type to 'summary'")
    }
    if(is.null(x) || length(result) == 0){
      rep(NA_character_, length(x))
    } else {
      as.character(result$tax_id[match(x, result$name_txt)])
    }
  } else {
    stop("The out_type value '", out_type, "' is not supported")
  }
}

itis_name2taxid <- function(src, x, empty, ...){
  stop("The ITIS database is currently not supported")
}

tpl_name2taxid <- function(src, x, empty, ...){
  stop("The TPL database is currently not supported")
}

col_name2taxid <- function(src, x, empty, ...){
  stop("The COL database is currently not supported")
}

gbif_name2taxid <- function(src, x, empty, ...){
  stop("The GBIF database is currently not supported")
}

ncbi_name2taxid <- function(src, x, empty, ...){
  if(length(x) == 0){
    return(empty)
  }
  # The NCBI taxonomy database includes common names, synonyms and
  # misspellings. However, the database is a little inconsistent here. For
  # some species, such as Arabidopsis thaliana, the misspelling
  # 'Arabidopsis_thaliana' is included, but the same is NOT done for humans.
  # However, underscores are supported when querying through entrez, which
  # implies they are replacing underscores with spaces. So I do the same.
  s = gsub('_', ' ', tolower(x))

  # FYI: The schema is set to support case insensitive matches
  query <- "SELECT name_txt, tax_id FROM names WHERE name_txt IN (%s)"
  query <- sprintf(query, paste(paste0("'", s, "'"), collapse=', '))
  result <- sql_collect(src, query)
  # There may be ambiguities
  result$name_txt <- x[pmatch(tolower(result$name_txt), s, duplicates.ok=TRUE)]

  # sort results first according to input order of names and second by taxon
  # order (which matters only for ambiguous entries)
  result <- result[order(factor(result$name_txt, levels=unique(x)), result$tax_id), ]
  result$tax_id <- as.character(result$tax_id)
  # There can be repeated rows, for example 'Bacteria' and 'bacteria' are both
  # are converted into 'Bacteria', but they point to the same taxon id.
  result <- dplyr::distinct(result)
  result
}
