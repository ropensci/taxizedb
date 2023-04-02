#' Retrieve all taxa descending from a vector of taxa
#'
#' This function is nearly equivalent to the `taxize::downstream()` function
#'
#' @export
#' @param x (character) Vector of taxon keys for the given database
#' @param db (character) The database to search, one of ncbi, itis, 
#' gbif, col, or wfo
#' @param verbose (logical) Print verbose messages
#' @param ... Additional arguments passed to database specific downstream
#' functions
#' @return list of data.frames with the columns: childtaxa_id, childtaxa_name,
#' and rank. This is exactly equivalent to the output of `taxize::downstream()`
#' @examples \dontrun{
#' # get descendents from all ranks
#' # downstream(c(3700, 9605)) # takes a while
#'
#' # limit results to species
#' downstream(c(3700, 9605), downto='species')
#'
#' # allow ambiguous nodes but no ambiguous species
#' downstream(
#'   c(3700, 9605),
#'   downto='species',
#'   ambiguous_nodes=FALSE,
#'   ambiguous_species=TRUE
#' )
#' 
#' # ITIS
#' id <- name2taxid('Aves', db = "itis")
#' downstream(id, db = "itis", downto = "family")
#' downstream(id, db = "itis", downto = "genus")
#' id <- name2taxid('Bombus', db = "itis")
#' downstream(id, db = "itis", downto = "species")
#' 
#' # COL
#' id <- name2taxid('Chordata', db = "col")
#' downstream(id, db = "col", downto = "family")
#' 
#' # GBIF
#' id <- name2taxid('Pinaceae', db = "gbif")
#' downstream(id, db = "gbif", downto = "genus")
#' 
#' # World Flora Online
#' id <- name2taxid('Pinaceae', db = "wfo")
#' downstream(id, db = "wfo", downto = "species")
#' }
downstream <- function(x, db='ncbi', verbose=TRUE, ...){
  ap_dispatch(
    x       = x,
    db      = db,
    cmd     = 'downstream',
    verbose = verbose,
    ...
  )
}

itis_downstream <- function(src, x, ...){
  FUN <- function(x, src, downto = NULL, ...) {
    if (taxid2rank(x, db = "itis") == downto) return(data.frame(NULL))
    downtoid <- itis_rankname2taxid(src, downto)
    stop_ <- "not"
    notout <- data.frame(rank = "")
    out <- list()
    iter <- 0
    while (stop_ == "not") {
      iter <- iter + 1
      if (!nchar(as.character(notout$rank[[1]])) > 0) {
        temp <- children(as.character(x), db = "itis")[[1]]
      } else {
        temp <- notout
      }
      
      tt <- temp
      if (!all(tolower(temp$rank) == tolower(downto))) {
        temp <- temp[temp$rank_id < downtoid, ]
        tt <- tt[tt$rank_id == downtoid, ]
        if (NROW(temp) > 0) {
          res <- children(temp$id, db = "itis")
          if (any(vapply(res, function(z) NROW(z) > 0, logical(1)))) {
            res <- Filter(function(z) NROW(z) > 0, res)
            tt <- dplyr::bind_rows(tt, dplyr::bind_rows(res))
          }
        }
      }
      
      if (NROW(tt[tt$rank_id == downtoid, ]) > 0) {
        out[[iter]] <- tt[tt$rank_id == downtoid, ]
      }

      if (NROW(tt[!tt$rank_id == downtoid, ]) > 0) {
        notout <- tt[!tt$rank_id %in% downtoid, ]
      } else {
        notout <- data.frame(rank = downto, stringsAsFactors = FALSE)
      }

      if (length(notout$rank) > 0)
        notout$rank <- tolower(notout$rank)
      if (all(notout$rank == tolower(downto))) {
        stop_ <- "fam"
      } else {
        x <- notout$id
        stop_ <- "not"
      }
    }
    tmp <- dplyr::bind_rows(out)
    tmp$rank_id <- NULL
    tmp$rank <- tolower(tmp$rank)
    stats::setNames(tmp, tolower(names(tmp)))
  }
  stats::setNames(lapply(x, FUN, src = src, ...), x)
}

tpl_downstream <- function(src, x, ...){
  stop("The TPL database is not supported")
}

col_downstream <- function(src, x, ...){
  FUN <- function(x, src, downto = NULL, ...) {
    if (taxid2rank(x, db = "col") == downto) return(data.frame(NULL))
    downtoid <- txdb_which_rank(downto)
    stop_ <- "not"
    notout <- data.frame(rank = "")
    out <- list()
    iter <- 0
    while (stop_ == "not") {
      iter <- iter + 1
      if (!all(tolower(notout$rank) == tolower(downto))) {
        res <- children(x, db = "col")
        if (any(vapply(res, function(z) NROW(z) > 0, logical(1)))) {
          res <- Filter(function(z) NROW(z) > 0, res)
          tt <- dplyr::bind_rows(res)
        }
      }
      
      tt_rank_ids <- txdb_which_rank_v(tt$rank)
      if (NROW(tt[tt_rank_ids == downtoid, ]) > 0) {
        out[[iter]] <- tt[tt_rank_ids == downtoid, ]
      }

      if (NROW(tt[!tt_rank_ids == downtoid, ]) > 0) {
        notout <- tt[!tt_rank_ids %in% downtoid, ]
      } else {
        notout <- data.frame(rank = downto, stringsAsFactors = FALSE)
      }

      if (length(notout$rank) > 0) {
        notout$rank <- tolower(notout$rank)
      }
      if (all(notout$rank == tolower(downto))) {
        stop_ <- "fam"
      } else {
        x <- notout$id
        stop_ <- "not"
      }
    }
    tmp <- dplyr::bind_rows(out)
    tmp$rank <- tolower(tmp$rank)
    stats::setNames(tmp, tolower(names(tmp)))
  }
  stats::setNames(lapply(x, FUN, src = src, ...), x)
}

wfo_downstream <- function(src, x, ...){
  FUN <- function(x, src, downto = NULL, ...) {
    if (taxid2rank(x, db = "wfo") == downto) return(data.frame(NULL))
    downtoid <- txdb_which_rank(downto)
    stop_ <- "not"
    notout <- data.frame(rank = "")
    out <- list()
    iter <- 0
    while (stop_ == "not") {
      iter <- iter + 1
      if (!all(tolower(notout$rank) == tolower(downto))) {
        res <- children(x, db = "wfo")
        if (any(vapply(res, function(z) NROW(z) > 0, logical(1)))) {
          res <- Filter(function(z) NROW(z) > 0, res)
          tt <- dplyr::bind_rows(res)
          tt <- txdb_prune_too_low(tt, downto)
        }
      }
      
      tt_rank_ids <- txdb_which_rank_v(tt$rank)
      if (NROW(tt[tt_rank_ids == downtoid, ]) > 0) {
        out[[iter]] <- tt[tt_rank_ids == downtoid, ]
      }

      if (NROW(tt[!tt_rank_ids == downtoid, ]) > 0) {
        notout <- tt[!tt_rank_ids %in% downtoid, ]
      } else {
        notout <- data.frame(rank = downto, stringsAsFactors = FALSE)
      }

      if (length(notout$rank) > 0) {
        notout$rank <- tolower(notout$rank)
      }
      if (all(notout$rank == tolower(downto))) {
        stop_ <- "fam"
      } else {
        x <- notout$id
        stop_ <- "not"
      }
    }
    tmp <- dplyr::bind_rows(out)
    tmp$rank <- tolower(tmp$rank)
    stats::setNames(tmp, tolower(names(tmp)))
  }
  stats::setNames(lapply(x, FUN, src = src, ...), x)
}

gbif_downstream <- function(src, x, ...){
  FUN <- function(x, src, downto = NULL, ...) {
    if (taxid2rank(x, db = "gbif") == downto) return(data.frame(NULL))
    downtoid <- txdb_which_rank(downto)
    stop_ <- "not"
    notout <- data.frame(rank = "")
    out <- list()
    iter <- 0
    while (stop_ == "not") {
      iter <- iter + 1
      if (!all(tolower(notout$rank) == tolower(downto))) {
        res <- children(x, db = "gbif")
        if (any(vapply(res, function(z) NROW(z) > 0, logical(1)))) {
          res <- Filter(function(z) NROW(z) > 0, res)
          tt <- dplyr::bind_rows(res)
          tt <- txdb_prune_too_low(tt, downto)
        }
      }
      
      tt_rank_ids <- txdb_which_rank_v(tt$rank)
      if (NROW(tt[tt_rank_ids == downtoid, ]) > 0) {
        out[[iter]] <- tt[tt_rank_ids == downtoid, ]
      }

      if (NROW(tt[!tt_rank_ids == downtoid, ]) > 0) {
        notout <- tt[!tt_rank_ids %in% downtoid, ]
      } else {
        notout <- data.frame(rank = downto, stringsAsFactors = FALSE)
      }

      if (length(notout$rank) > 0) {
        notout$rank <- tolower(notout$rank)
      }
      if (all(notout$rank == tolower(downto))) {
        stop_ <- "fam"
      } else {
        x <- notout$id
        stop_ <- "not"
      }
    }
    tmp <- dplyr::bind_rows(out)
    tmp$rank <- tolower(tmp$rank)
    stats::setNames(tmp, tolower(names(tmp)))
  }
  stats::setNames(lapply(x, FUN, src = src, ...), x)
}

ncbi_downstream <- function(src, x, ...){

  FUN <- function(
    src,
    x,
    ambiguous_nodes   = FALSE,
    ambiguous_species = FALSE,
    downto            = NULL,
    ...
  ){
    cmd <- "SELECT tax_id, level, ancestor FROM hierarchy WHERE ancestor IN (%s)"
    query <- sprintf(cmd, sql_integer_list(x))
    sql_collect(src, query) %>%
      dplyr::mutate(
        rank = taxid2rank(.data$tax_id),
        childtaxa_name = taxid2name(.data$tax_id)
      ) %>%
      dplyr::select(
        childtaxa_id   = .data$tax_id,
        childtaxa_name = .data$childtaxa_name,
        rank           = rank,
        key            = .data$ancestor
      ) %>%
      {
        if(!is.null(downto)){
          dplyr::filter(., .data$rank == downto)
        } else {
          .
        }
      } %>%
      dplyr::filter(ambiguous_nodes   | (.$rank == 'species' | !is_ambiguous(.data$childtaxa_name))) %>%
      dplyr::filter(ambiguous_species | (.$rank != 'species' | !is_ambiguous(.data$childtaxa_name))) %>%
      split(f=.$key) %>%
      lapply(function(d){
        d$key <- NULL
        d <-  dplyr::arrange(as.data.frame(d), -.data$childtaxa_id)
        d$childtaxa_id <- as.character(d$childtaxa_id)
        d
      })
  }

  ## TODO: probably the Right missing value is this:
  # missing = data.frame(
  #   childtaxa_id   = character(),
  #   childtaxa_name = character(),
  #   rank           = character(),
  #   stringsAsFactors=FALSE
  # )
  missing = NA

  ncbi_apply(src, x, FUN, missing=missing, ...)
}

itis_rankname2taxid <- function(src, x) {
  ranks <- unique(sql_collect(src,
    'select rank_id,rank_name from taxon_unit_types'))
  ranks[tolower(ranks$rank_name) %in% tolower(x), ]$rank_id
}
