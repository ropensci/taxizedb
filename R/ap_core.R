### Internal utilities

ap_vector_dispatch <- function(x, db, cmd, verbose=TRUE, empty=character(0), ...){
  if(is.null(x) || length(x) == 0){
    empty 
  } else {
    FUN <- paste0(db, "_", cmd)
    run_with_db(FUN=get(FUN), db=db, x=x, ...)
  }
}

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

ncbi_apply <- function(src, x, FUN, missing=NA, ...){
  # preserve original names (this is important when x is a name vector)
  namemap <- x
  names(namemap) <- x
  # find the entries that are named
  is_named <- !(grepl('^[0-9]+$', x, perl=TRUE) | is.na(x))
  # If x is not integrel, then we assume it is a name.
  if(any(is_named)){
    x[is_named] <- name2taxid(x[is_named], db='ncbi')
    names(namemap)[is_named] <- x[is_named]
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
    missing_values <- lapply(missing_names, function(x) missing)
    names(missing_values) <- missing_names
    result <- append(result, missing_values)
  }
  # Get result in the input order
  result <- result[as.character(namemap)]
  # Cleanup residual NULLs (if needed)
  result <- lapply(result, function(x) {
    if(is.null(x)){ NA } else { x }
  })
  result[is.na(names(result))] <- missing
  result
}
