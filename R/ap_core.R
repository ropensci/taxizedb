### Internal utilities

ap_dispatch <- function(x, db, cmd, out_class=cmd, verbose=TRUE, ...){
  result <- if(is.null(x) || length(x) == 0){
    # For empty or NULL input, return empty list
    list()
  } else {
    FUN <- paste0(db, "_", cmd)
    run_with_db(FUN=get(FUN), db=db, x=x, ...)
  }

  attributes(result) <- list(names=names(result), class=out_class, db=db)

  if(verbose && all(is.na(result))){
    message("No results found. Consider checking the spelling or alternative classification")
  }

  result 
}

run_with_db <- function(FUN, db, ...){
  src <- if(db == 'ncbi'){
    src_ncbi(db_download_ncbi())
  } else {
    stop("Database '", db, "' is not supported")
  }
  FUN(src, ...)
}

ncbi_apply <- function(src, x, FUN, ...){
  # preserve original names (this is important when x is a name vector)
  namemap <- x
  names(namemap) <- x
  # find the entries that are integers
  not_integer <- !grepl('^[0-9]+$', x, perl=TRUE)
  # If x is not integrel, then we assume it is a name.
  if(any(not_integer)){
    # The NCBI taxonomy database includes common names, synonyms and
    # misspellings. However, the database is a little inconsistent here. For
    # some species, such as Arabidopsis thaliana, the misspelling
    # 'Arabidopsis_thaliana' is included, but the same is NOT done for humans.
    # However, underscores are supported when querying through entrez, which
    # implies they are replacing underscores with spaces. So I do the same.
    s <- x[not_integer]
    s <- gsub('_', ' ', s)
    # FYI: The schema is set to support case insensitive matches
    query <- "SELECT name_txt, tax_id FROM names WHERE name_txt IN (%s)"
    query <- sprintf(query, paste(paste0("'", s, "'"), collapse=', '))
    result <- sql_collect(src, query)
    result$name_txt <- tolower(result$name_txt)
    was_found <- gsub('_', ' ', tolower(namemap)) %in% result$name_txt
    x[was_found] <- result$tax_id[order(match(result$name_txt, tolower(namemap)))]
    names(namemap)[was_found] <- x[was_found]
    x[not_integer & !was_found] <- NA
  }
  # Remove any taxa that where not found, the missing values will be merged
  # into the final output later (with values of NA)
  x <- x[!is.na(x)]

  # If there isn't anything to consider, return a list of NA
  if(length(x) == 0){
    result <- as.list(as.logical(rep(NA, length(namemap))))
    names(result) <- namemap
    return(result)
  }


  # Run the given function on the clean x
  result <- FUN(src, x, ...)


  # Map the input names to them
  names(result) <- namemap[names(result)]
  # Add the missing values
  missing_names <- setdiff(namemap, names(result))
  if(length(missing_names) > 0){
    missing <- as.list(as.logical(rep(NA, length(missing_names))))
    names(missing) <- missing_names
    result <- append(result, missing)
  }
  # Get result in the input order
  result <- result[as.character(namemap)]
  # Cleanup residual NULLs (if needed)
  result <- lapply(result, function(x) {
    if(is.null(x)){ NA } else { x }
  })
  result[is.na(names(result))] <- NA
  result
}
