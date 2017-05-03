context("cache")

test_that("tdb_cache structure is as expected", {
  expect_is(tdb_cache, "HoardClient")
  expect_is(tdb_cache, "R6")
  expect_is(tdb_cache$cache_path_get, "function")
  expect_is(tdb_cache$cache_path_set, "function")
  expect_is(tdb_cache$compress, "function")
  expect_is(tdb_cache$uncompress, "function")
  expect_is(tdb_cache$delete, "function")
  expect_is(tdb_cache$delete_all, "function")
  expect_is(tdb_cache$details, "function")
  expect_is(tdb_cache$files, "function")
  expect_is(tdb_cache$key, "function")
  expect_is(tdb_cache$keys, "function")
  expect_is(tdb_cache$list, "function")
  expect_is(tdb_cache$mkdir, "function")
  expect_is(tdb_cache$print, "function")

  expect_is(tdb_cache$path, "character")
  expect_null(tdb_cache$type)
})


test_that("tdb_cache works as expected", {
  skip_on_cran()

  # clear cache in case any in there
  tdb_cache$delete_all()

  # cache should be empty
  expect_equal(length(tdb_cache$list()), 0)

  # message on delete all
  expect_message(tdb_cache$delete_all(), "no files found")

  # message on delete all
  expect_error(tdb_cache$delete(), "argument \"files\" is missing")
  expect_error(tdb_cache$delete("adfdf"), "These files don't exist")

  # details, with no files
  expect_is(tdb_cache$details(), "cache_info")

  # files is NULL
  expect_null(tdb_cache$files())

  # keys is NULL
  expect_null(tdb_cache$keys())

  # key is NULL and errors well when file not found
  expect_error(tdb_cache$key(), "argument \"x\" is missing")
  expect_error(tdb_cache$key("ADfdf"), "file does not exist")

  # path is default
  expect_equal(tdb_cache$path, "taxizedb")
})
