#' ITIS Downstream
#' @param src database src
#' @param x taxonomic id
#' @examples \dontrun{
#' (src <- src_itis())
#' 
#' # children
#' (id <- name2taxid('Aves', db = "itis"))
#' (out <- itis_children(src, x = id))
#' 
#' (id <- name2taxid('Apis', db = "itis"))
#' (out <- itis_children(src, x = id))
#' 
#' (id <- name2taxid(c('Apis', 'Lasioglossum'), db = "itis"))
#' (out <- itis_children(src, x = id))
#' 
#' # downstream
#' (id <- name2taxid('Aves', db = "itis"))
#' itis_downstream(src, x = id, downto = "family")
#' z <- itis_downstream(src, x = id, downto = "genus")
#' w <- itis_downstream(src, x = id, downto = "species")
#' }

# itis_children2 <- function(src, x) {
#   FUN <- function(x, src) {
#     ranks <- unique(sql_collect(src, 'select * from taxon_unit_types'))
#     children <- 
#       sql_collect(src, sprintf("select * from hierarchy where Parent_TSN = '%s'", x))
#     tsns <- children$TSN
#     child_query <- sprintf(
#       "SELECT tsn,rank_id,complete_name FROM taxonomic_units WHERE tsn IN ('%s')", 
#       paste0(tsns, collapse = "','"))
#     child_df <- sql_collect(src, child_query)
#     unique(dplyr::left_join(child_df, 
#       dplyr::select(ranks, rank_id, rank_name)))
#   }
#   stats::setNames(lapply(x, FUN, src = src), x)
# }

# itis_downstream <- function(src, x, downto, ...) {
#   downtoid <- itis_rankname2taxid(downto)
#   stop_ <- "not"
#   notout <- data.frame(rank_name = "")
#   out <- list()
#   iter <- 0
#   while (stop_ == "not") {
#     iter <- iter + 1
#     if (!nchar(as.character(notout$rank_name[[1]])) > 0) {
#       temp <- children(as.character(x), db = "itis")[[1]]
#     } else {
#       temp <- notout
#     }
    
#     tt <- dplyr::bind_rows(children(temp$tsn, db = "itis"))
    
#     if (NROW(tt[tt$rank_id == downtoid, ]) > 0) {
#       out[[iter]] <- tt[tt$rank_id == downtoid, ]
#     }

#     if (NROW(tt[!tt$rank_id == downtoid, ]) > 0) {
#       notout <- tt[!tt$rank_id %in% downtoid, ]
#     } else {
#       notout <- data.frame(rank_name = downto, stringsAsFactors = FALSE)
#     }

#     if (length(notout$rank_name) > 0)
#       notout$rank_name <- tolower(notout$rank_name)
#     if (all(notout$rank_name == tolower(downto))) {
#       stop_ <- "fam"
#     } else {
#       tsns <- notout$tsn
#       stop_ <- "not"
#     }
#   }
#   tmp <- dplyr::bind_rows(out)
#   tmp$rank_name <- tolower(tmp$rank_name)
#   stats::setNames(tmp, tolower(names(tmp)))
# }

