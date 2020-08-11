#' Convert species names to taxon IDs
#'
#' `name2taxid()` returns a vector and dies if there are any ambiguous
#' names. `name2taxid_map()` returns a data.frame mapping names to ids
#'
#' @section NCBI database:
#'
#' The NCBI taxonomy database includes common names, synonyms and misspellings.
#' However, the database is a little inconsistent. For some species, such as
#' Arabidopsis thaliana, the misspelling Arabidopsis_thaliana is included, but
#' the same is NOT done for humans. However, underscores are supported when
#' querying through entrez, as is done in taxize, which implies entrez is
#' replacing underscores with spaces. So I do the same. A corner case appears
#' when an organism uses underscores as part of the name, not just a standin
#' for space ("haloarchaeon 3A1_DGR"). To deal with this case, we replace
#' underscores with spaces ONLY if there are not spaces in the original name. 
#'
#' @export
#' @param x (character) Vector of taxon keys for the given database
#' @param db (character) The database to search, one of ncbi or itis
#' @param verbose (logical) Print verbose messages
#' @param out_type (logical) character "uid" for an ID vector, "summary" for a
#' table with columns 'tax_id' and 'tax_name'.
#' @param ... Additional arguments passed to database specific classification
#' functions.
#' @examples \dontrun{
#' name2taxid(c('Arabidopsis thaliana', 'pig'))
#' name2taxid(c('Arabidopsis thaliana', 'pig'), out_type="summary")
#' name2taxid(x=c('Arabidopsis thaliana', 'Apis mellifera'), db = "itis")
#' name2taxid(x=c('Arabidopsis thaliana', 'Apis mellifera'), db = "itis",
#'   out_type="summary")
#' }
name2taxid <- function(x, db='ncbi', verbose=TRUE,
  out_type=c("uid", "summary"), ...) {

  result <- ap_vector_dispatch(
    x       = x,
    db      = db,
    cmd     = 'name2taxid',
    verbose = verbose,
    empty   = tibble::tibble(name = character(), id = character()),
    ...
  )
  if(identical(out_type[1], "summary")){
    result
  } else if (identical(out_type[1], "uid")) {
    ids <- result$id
    if(any(duplicated(result$name))){
      stop("Some of the input names are ambiguous, try setting out_type to 'summary'")
    }
    if(is.null(x) || length(result) == 0){
      rep(NA_character_, length(x))
    } else {
      as.character(result$id[match(x, result$name)])
    }
  } else {
    stop("The out_type value '", out_type, "' is not supported")
  }
}

itis_name2taxid <- function(src, x, empty, ...){
  if (length(x) == 0) return(empty)
  query <- "SELECT tsn,complete_name FROM taxonomic_units WHERE complete_name IN ('%s')"
  query <- sprintf(query, paste0(x, collapse = "','"))
  result <- sql_collect(src, query)
  vars <- c(id = "tsn", name = "complete_name")
  result <- dplyr::rename(result, vars)
  result
}

ncbi_name2taxid <- function(src, x, empty, ...){
  if (length(x) == 0) return(empty)

  # x is saved to preserve the input name (e.g. an alternative spelling)
  s <- tolower(x)
  has_no_space <- !grepl(" ", s)
  s[has_no_space] <- gsub("_", " ", s[has_no_space])

  # FYI: The schema is set to support case insensitive matches
  query <- "SELECT name_txt, tax_id FROM names WHERE name_txt IN (%s)"
  query <- sprintf(query, sql_character_list(s))
  result <- sql_collect(src, query)
  # There may be ambiguities
  result$name_txt <- x[pmatch(tolower(result$name_txt), s, duplicates.ok=TRUE)]

  # sort results first according to input order of names and second by taxon
  # order (which matters only for ambiguous entries)
  result <- result[order(factor(result$name_txt, levels=unique(x)),
    result$tax_id), ]
  result$tax_id <- as.character(result$tax_id)
  # There can be repeated rows, for example 'Bacteria' and 'bacteria' are both
  # are converted into 'Bacteria', but they point to the same taxon id.
  result <- dplyr::distinct(result)
  vars <- c(id = "tax_id", name = "name_txt")
  result <- dplyr::rename(result, vars)
  result
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
