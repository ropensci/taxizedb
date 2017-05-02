#' Set location of data acquisition.
#'
#' @name backend
#' @param itis,col,ncbi,theplantlist (character) The backend to query data
#' from. Defaults to `api`, which means we query resources on the web.
#' Alternatively, use `local`, which uses local versions of databases on
#' your own machine. This option does some checks to make sure things are
#' setup correctly.
#' @param path The path to store local databases. Individual databases are
#' within this base path.
#' @param itis_user ITIS user name
#' @param itis_pwd ITIS password
#' @param col_user Catalogue of Life user name
#' @param col_pwd Catalogue of Life password
#'
#' @details All other data source parameters in [backend_set()] inherit
#' from the first `itis`, which is by default set to `itis = "api"`
#'
#' Note that the only option for Theplantlist data is "local". Alternatively,
#' you can download raw csv files of their data using
#' [taxize::tpl_get()], or use the \pkg{Taxonstand} package that
#' downloads csv files and uses regex locally in R.
#' @examples \dontrun{
#' # set all to remote api
#' backend_set("api")
#' backend_get()
#'
#' # set all to local
#' backend_set("local")
#' backend_get()
#'
#' # set individual data sources to different settings
#' backend_set(col = "local", ncbi = "local")
#' backend_get()
#'
#' # Set username and password for COL connection
#' backend_set(col_user="root")
#' }

#' @export
#' @rdname backend
backend_set <- function(itis = "api", col = itis, ncbi = itis,
  theplantlist = itis, path="~/.taxize_local", itis_user = NULL,
  itis_pwd = NULL, col_user = NULL, col_pwd = NULL){

  invisible(sapply(list(itis, col, ncbi), mb))
  options(itis_backend = itis)
  options(itis_user = itis_user)
  options(itis_pwd = itis_pwd)
  options(col_backend = col)
  options(col_user = col_user)
  options(col_pwd = col_pwd)
  options(ncbi_backend = ncbi)
  options(theplantlist_backend = theplantlist)
  options(taxize_path = path)
}

#' @export
#' @rdname backend
backend_get <- function(){
  bends <- c("itis_backend", "col_backend", "ncbi_backend",
             "theplantlist_backend", "taxize_path",
             "col_user", "col_pwd")
  structure(lapply(bends, getOption), class = "taxize_backends",
            .Names = bends)
}

#' @export
print.taxize_backends <- function(x, ...){
  cat(paste0("<taxizedb backends>  ", x$taxize_path), sep = "\n")
  cat(paste0("  ITIS: ", x$itis_backend), sep = "\n")
  cat(paste0("  COL: ", x$col_backend), sep = "\n")
  cat(paste0("  NCBI: ", x$ncbi_backend), sep = "\n")
  cat(paste0("  ThePlantList: ", x$theplantlist_backend), sep = "\n")
}

mb <- function(x){
  match.arg(x, c("api","local"))
}

make_path <- function(x, y) path.expand(file.path(y, paste0(x, ".sqlite")))
