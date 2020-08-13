### Internal utilities

# convert a vector to a comma separated string list suitable for SQL, e.g.
# c("Arabidopsis", "Peridermium sp. 'Ysgr-4'") -> "'Arabidopsis',
# 'Peridermium sp. ''Ysgr-4'''"
# Note that double quoting is the SQL convention for escaping quotes in strings
sql_character_list <- function(x){
  if(any(is.na(x))){
    stop("Cannot pass NA into SQL query")
  }
  as.character(x) %>%
    gsub(pattern="'", replacement="''") %>%
    sub(pattern="(.*)", replacement="'\\1'", perl=TRUE) %>%
    paste(collapse=", ")
}

sql_integer_list <- function(x){
  if(any(is.na(x))){
    stop("Cannot pass NA into SQL query")
  }
  x <- as.character(x)
  if(!all(grepl('^[0-9]+$', x, perl=TRUE))){
    stop("Found non-integer where integer required in SQL input")
  }
  paste(x, collapse=", ")
}

ap_vector_dispatch <- function(x, db, cmd, verbose=TRUE, empty=character(0), ...){
  if(is.null(x) || length(x) == 0){
    empty 
  } else {
    FUN <- paste0(db, "_", cmd)
    run_with_db(FUN=get(FUN), db=db, x=x, empty=empty, ...)
  }
}

ap_dispatch <- function(x, db, cmd, out_class=cmd, empty=list(), verbose=TRUE, ...){
  assert(db, "character")
  assert(verbose, "logical")
  result <- if(is.null(x) || length(x) == 0) {
    # For empty or NULL input, return empty list
    empty
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

run_with_db <- function(FUN, db, ...) {
  src <- if(db == 'ncbi') {
    src_ncbi(db_download_ncbi(verbose = FALSE))
  } else if (db == "itis") {
    src_itis(db_download_itis(verbose = FALSE))
  } else if (db == "wfo") {
    src_itis(db_download_wfo(verbose = FALSE))
  } else if (db == "gbif") {
    src_itis(db_download_gbif(verbose = FALSE))
  } else if (db == "col") {
    src_itis(db_download_col(verbose = FALSE))
  } else if (db == "tpl") {
    src_itis(db_download_tpl(verbose = FALSE))
  } else {
    stop("Database '", db, "' is not supported")
  }
  FUN(src, ...)
}

ncbi_apply <- function(src, x, FUN, missing=NA, die_if_ambiguous=TRUE, ...){
  # preserve original names (this is important when x is a name vector)
  namemap <- x
  names(namemap) <- x
  # find the entries that are named
  is_named <- !(grepl('^[0-9]+$', x, perl=TRUE) | is.na(x))
  # If x is not integrel, then we assume it is a name.
  if(any(is_named)){

    # This is not a pretty solution, since it makes an unnecessary call to the database
    if(die_if_ambiguous){
      # get a table mapping names to taxa
      d <- name2taxid(x[is_named], db='ncbi', out_type="summary")
      # find duplicated elements (ambiguous taxa)
      dups <- unique(d$name[duplicated(d$name)])
      # die if there are any
      if(length(dups) > 0){
        msg <- "The following taxa map to multiple taxonomy ids: "
        msg <- dplyr::group_by(d, .data$name) %>%
          dplyr::filter(length(.data$name) > 1) %>%
          dplyr::summarize(taxids = paste(.data$tax_id, collapse="|")) %>% {
            paste("    ", .$name, " - ", .$taxids, collapse="\n")
          } %>%
          paste(msg, ., sep="\n")
        stop(msg)
      }
    }

    # get a table mapping names to taxa
    x[is_named] <- name2taxid(x[is_named], db='ncbi')
    names(namemap)[is_named] <- x[is_named]
  }
  # Remove any taxa that where not found, the missing values will be merged
  # into the final output later (with values of NA)
  x <- x[!is.na(x)]

  # If there isn't anything to consider, return a list of NA
  if(length(x) == 0){
    result <- as.list(as.logical(rep(NA, length(namemap))))
    result <- lapply(result, function(x) missing)
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

is_ambiguous <- function(scinames){
  # ambiguous terms (see taxize::ncbi_children.R)
  ambiguous_regex <- paste0(c("unclassified", "environmental", "uncultured", "unknown",
                       "unidentified", "candidate", "sp\\.", "s\\.l\\.", "sensu lato", "clone",
                       "miscellaneous", "candidatus", "affinis", "aff\\.", "incertae sedis",
                       "mixed", "samples", "libaries"), collapse="|")
  grepl(ambiguous_regex, scinames, perl=TRUE)
}
