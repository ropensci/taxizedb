#' Convert species names to taxon IDs
#'
#' \code{name2taxid} returns a vector and dies if there are any ambiguous
#' names. \code{name2taxid_map} returns a data.frame mapping names to ids.
#'
#' @param x Vector of taxon keys for the given database
#' @param db The database to search
#' @param verbose Print verbose messages
#' @param ... Additional arguments passed to database specific classification functions.
#' @examples
#' \dontrun{
#' name2taxid(c('Arabidopsis thaliana', 'pig'))
#' }
#' @name name2taxid
NULL

#' @rdname name2taxid
#' @export
name2taxid <- function(x, db='ncbi', verbose=TRUE, ...){
  result <- name2taxid_map(x, db, verbose, ...)
  if(length(result) == 0){
    return(result)
  } else {
    if(length(unique(result$tax_id)) != nrow(result)){
      stop("Some of these names are ambiguous, so this cannot be simplified ",
           "to a vector, try setting 'simplify=FALSE'")
    }
    as.character(result$tax_id[match(x, result$name_txt)])
  }
}

#' @rdname name2taxid
#' @export
name2taxid_map <- function(x, db='ncbi', verbose=TRUE, ...){
  ap_vector_dispatch(x=x, db=db, cmd='name2taxid', verbose=verbose, ...)
}

itis_name2taxid <- function(src, x, ...){
  stop("The ITIS database is currently not supported")
}

tpl_name2taxid <- function(src, x, ...){
  stop("The TPL database is currently not supported")
}

col_name2taxid <- function(src, x, ...){
  stop("The COL database is currently not supported")
}

gbif_name2taxid <- function(src, x, ...){
  stop("The GBIF database is currently not supported")
}

ncbi_name2taxid <- function(src, x, ...){
  if(length(x) == 0){
    return(character(0))
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
  # sort results according to input order
  result <- result[order(pmatch(x, result$name_txt)), ]
  result
}
