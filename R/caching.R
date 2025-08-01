#' @title Caching
#'
#' @description Manage cached taxizedb files with \pkg{hoardr}
#'
#' @export
#' @name tdb_cache
#' 
#' @return An object of classes `HoardClient` and `R6`.
#'
#' @details `cache_delete` only accepts 1 file name, while
#' `cache_delete_all` doesn't accept any names, but deletes all files.
#' For deleting many specific files, use `cache_delete` in a [lapply()]
#' type call
#'
#' @section Useful user functions:
#' 
#' - `tdb_cache$cache_path_get()` get cache path
#' - `tdb_cache$cache_path_set()` set cache path
#' - `tdb_cache$list()` returns a character vector of full
#'    path file names
#' - `tdb_cache$files()` returns file objects with metadata
#' - `tdb_cache$details()` returns files with details
#' - `tdb_cache$delete()` delete specific files
#' - `tdb_cache$delete_all()` delete all files, returns nothing
#'
#' @examples \dontrun{
#' tdb_cache
#'
#' # list files in cache
#' tdb_cache$list()
#'
#' # delete certain database files
#' # tdb_cache$delete("file path")
#' # tdb_cache$list()
#'
#' # delete all files in cache
#' # tdb_cache$delete_all()
#' # tdb_cache$list()
#' }
NULL
