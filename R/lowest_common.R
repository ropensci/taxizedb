#' Retrieve the lowest common taxon and rank for a given taxon name or ID
#'
#' @export
#' @param x Vector of taxonomic ids (character or numeric) 
#' @param db character; database to query. only `ncbi` for now
#' @param class_list (list) A list of classifications, as returned from
#' [classification()]
#' @param low_rank (character) taxonomic rank to return, of length 1
#'
#' @return NA when no match, or a data.frame with columns
#' \itemize{
#'  \item name
#'  \item rank
#'  \item id
#' }
#' @examples \dontrun{
#' id <- c(9031, 9823, 9606, 9470)
#' id_class <- classification(id, db = 'ncbi')
#' lowest_common(id, db = "ncbi")
#' lowest_common(id[2:4], db = "ncbi")
#' lowest_common(id[2:4], db = "ncbi", low_rank = 'class')
#' lowest_common(id[2:4], db = "ncbi", low_rank = 'family')
#' lowest_common(id[2:4], class_list = id_class)
#' lowest_common(id[2:4], class_list = id_class, low_rank = 'class')
#' lowest_common(id[2:4], class_list = id_class, low_rank = 'family')
#' 
#' 
#' id <- c(72, 562, 786, 948, 950, 1046, 1106, 1117, 1213, 1224, 1239, 1392, 
#' 2138, 2708, 2709, 2711, 2759, 2763, 2769, 2825, 2830, 2836, 2864, 2889, 
#' 3035, 3118, 3133, 3146, 3193, 3195)
#' id_class <- classification(id, db = 'ncbi')
#' lowest_common(id, db = "ncbi")
#' }
lowest_common <- function(x, db = NULL, ...){
  UseMethod("lowest_common")
}

#' @export
#' @rdname lowest_common
lowest_common.default <- function(x, db = NULL, class_list = NULL,
                                  low_rank = NULL, ...) {
  if (is.null(db)) if (!is.null(class_list)) db <- attr(class_list, "db")
  # nstop(db)
  switch(
    db,
    ncbi = {
      # id <- process_lowest_ids(x, db, get_uid, rows = rows, ...)
      lowest_common_ncbi(x, class_list, ...)
    },
    stop("the provided db value was not recognised", call. = FALSE)
  )
}

lowest_common_ncbi <- function(x, class_list = NULL, low_rank = NULL, ...) {
  check_lowest_ids(x)
  class_list <- get_class(x, class_list, db = "ncbi")
  lc_helper(x, class_list, low_rank)
}

# helpers -------------------------------------------------
lc_helper <- function(x, class_list, low_rank = NULL, ...) {
  idc <- class_list[x %in% names(class_list)]
  # next line NCBI specific
  cseq <- vapply(idc, function(x) x[1, 1] != "unclassified sequences",
                 logical(1))
  idc <- idc[cseq]
  if (is.null(low_rank)) {
    x_row <- length(Reduce(intersect, lapply(idc, "[[", 1)))
    x <- idc[[1]][x_row, ]
    if (x[1, "rank"] == "no rank") {
      x[1, "rank"] <- next_best_taxon(idc[[1]][1:x_row, ])
    }
    return(x)
  } else {
    valid_ranks <- tolower(ritis::rank_names()$rankname)
    if (!(low_rank %in% valid_ranks)) {
      warning('the supplied rank is not valid')
    }
    # low_rank_names <- as.character(unique(unlist(lapply(idc, function(x) x$name[which(x$rank == low_rank)]))))
    low_rank_names <- unique(setDF(rbindlist(lapply(idc, function(x)
      x[which(x$rank == low_rank),]))))
    if (NROW(low_rank_names) == 1) {
      return(low_rank_names)
    } else {
      return(NA)
    }
  }
}

next_best_taxon <- function(x){
  paste("below-",
        tail(x[, "rank"][!duplicated(x[, "rank"])], n = 1
        ), sep = "")
}

get_class <- function(x, y, db, ...) {
  if (is.null(y)) {
    classification(x, db = db, ...)
  } else {
    yattr <- stringr::str_replace(attr(y, "db"), "ncbi", "uid")
    if (!yattr %in% c('ncbi', 'uid')) {
      stop(sprintf("class_list input must be of class '%s'", db), 
        call. = FALSE)
    }
    y
  }
}

check_lowest_ids <- function(x) {
  notmiss <- na.omit(x)
  if (length(notmiss) < 2) {
    stop(length(notmiss), " found, ", length(x) - length(notmiss),
         " were NA; > 1 needed", call. = FALSE)
  }
}

nstop <- function(x, arg='db') if (is.null(x)) stop(sprintf("Must specify %s!", arg), call. = FALSE)
