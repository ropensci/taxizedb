#' Retrieve all taxa descending from a vector of taxa
#'
#' This function is nearly equivalent to the `taxize::downstream()` function
#'
#' @export
#' @param x (character) Vector of taxon keys for the given database
#' @param db (character) The database to search, one of ncbi or itis
#' @param verbose (logical) Print verbose messages
#' @param ... Additional arguments passed to database specific downstream
#' functions
#' @return list of data.frames with the columns: childtaxa_id, childtaxa_name,
#' and rank. This is exactly equivalent to the output of `taxize::downstream()`
#' @examples
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
#' (id <- name2taxid('Aves', db = "itis"))
#' downstream(id, db = "itis", downto = "family")
#' downstream(id, db = "itis", downto = "genus")
#' (id <- name2taxid('Bombus', db = "itis"))
#' downstream(id, db = "itis", downto = "species")
downstream <- function(x, db='ncbi', verbose=TRUE, ...){
  ap_dispatch(
    x       = x,
    db      = db,
    cmd     = 'downstream',
    ...
  )
}

itis_downstream <- function(src, x, ...){
  FUN <- function(x, src, downto = NULL, ...) {
    if (taxid2rank(x, db = "itis") == downto) return(data.frame(NULL))
    downtoid <- itis_rankname2taxid(src, downto)
    stop_ <- "not"
    notout <- data.frame(rank_name = "")
    out <- list()
    iter <- 0
    while (stop_ == "not") {
      iter <- iter + 1
      if (!nchar(as.character(notout$rank_name[[1]])) > 0) {
        temp <- children(as.character(x), db = "itis")[[1]]
      } else {
        temp <- notout
      }
      
      tt <- temp
      if (!all(tolower(temp$rank_name) == tolower(downto))) {
        temp <- temp[temp$rank_id < downtoid, ]
        tt <- tt[tt$rank_id == downtoid, ]
        if (NROW(temp) > 0) {
          res <- children(temp$tsn, db = "itis")
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
        notout <- data.frame(rank_name = downto, stringsAsFactors = FALSE)
      }

      if (length(notout$rank_name) > 0)
        notout$rank_name <- tolower(notout$rank_name)
      if (all(notout$rank_name == tolower(downto))) {
        stop_ <- "fam"
      } else {
        tsns <- notout$tsn
        stop_ <- "not"
      }
    }
    tmp <- dplyr::bind_rows(out)
    tmp$rank_id <- NULL
    tmp$rank_name <- tolower(tmp$rank_name)
    stats::setNames(tmp, tolower(names(tmp)))
  }
  missing = NA
  stats::setNames(lapply(x, FUN, src = src, ...), x)
}

tpl_downstream <- function(src, x, ...){
  stop("The TPL database is currently not supported")
}

col_downstream <- function(src, x, ...){
  stop("The COL database is currently not supported")
}

gbif_downstream <- function(src, x, ...){
  stop("The GBIF database is currently not supported")
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
        rank           = .data$rank,
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
