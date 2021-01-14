#' Get taxa at specific scientific ranks
#'
#' @export
#' @param x (character) Vector of taxon keys (ids) for the given
#' database. required
#' @param rank (character) A target rank for which to fetch data. required
#' @param db (character) The database to search, one of ncbi, itis, gbif,
#' col, or wfo
#' @param missing (character) if no data found at the given rank and input key,
#' should we get the next closest lower than that given in `rank`, or higher.
#' one of lower (default), higher.
#' @param verbose (logical) Print verbose messages
#' @param warn (logical) If `TRUE`, raise a warning if any taxon IDs can not
#' be found
#' @param ... Additional arguments passed to database specific classification
#' functions
#' @return list of data.frame's for each input taxon key, where each data.frame
#' has fields: name, rank, id. When no results found, an empty data.frame
#' @examples \dontrun{
#' taxa_at(186803, rank = "order", db="ncbi", missing = "lower")
#' taxa_at(c(186803, 541000, 216572, 186804, 31979,  186806),
#'  rank = "family", missing = "lower")
#' taxa_at(c(154395, 154357, 23041, 154396), rank = "family", db="itis")
#' taxa_at(c('wfo-4000032377', 'wfo-0000541830'), rank = "family", db="wfo")
#' taxa_at("wfo-7000000057", rank = "order", db="wfo")
#' taxa_at(2877951, rank = "phylum", db="gbif")
#' taxa_at(c(2877951, 5386), rank = "family", db="gbif")
#' taxa_at(c(3960765, 3953606, 3953010), rank = "family", db="col")
#' }
taxa_at <- function(x, rank, db='ncbi', missing = "lower", verbose=TRUE,
  warn=TRUE, ...) {

  ms_opts <- c("lower", "higher")
  if (!missing %in% ms_opts)
    stop("'missing' must be one of ", paste0(ms_opts, collapse=", "))
  ap_dispatch(
    x       = x,
    db      = db,
    cmd     = 'taxa_at',
    verbose = verbose,
    warn    = warn,
    empty   = data.frame(NULL),
    rank    = rank,
    missing = missing,
    ...
  )
}

ncbi_taxa_at <- function(src, x, rank, missing, ...) {
  tmp <- classification(x, db = src2db(src))
  FUN <- function(w, src, ...) {
    if (!is.data.frame(w)) return(data.frame(NULL))
    df <- w[rank == w$rank, ]
    if (NROW(df) == 1) return(df)
    if (NROW(df) == 0) {
      rowid <- txdb_which_rank(rank)
      if (missing == "lower") rks <- txdb_rr[(rowid+1):NROW(txdb_rr),"ranks"]
      if (missing == "higher") rks <- txdb_rr[1:(rowid-1),"ranks"]
      # find next closest rank
      mtchs <- rks[rks %in% w$rank]
      if (length(mtchs) == 0) return(data.frame(NULL))
      return(w[w$rank == mtchs[1],])
    }
  }
  stats::setNames(lapply(tmp, FUN, src = src), x)
}
gbif_taxa_at <- itis_taxa_at <- wfo_taxa_at <-
  col_taxa_at <- ncbi_taxa_at

src2db <- function(src) {
  path <- attr(src$con, "dbname")
  dbs <- c("col", "gbif", "ncbi", "wikidata", "wfo", "itis")
  dbs[vapply(dbs, grepl, logical(1), x = path, ignore.case = TRUE)]
}
