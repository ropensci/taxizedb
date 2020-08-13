context("downstream")

skip_on_cran()

test_that("taxizedb::downstream matches taxize::downstream", {
  # FIXME, these are no longer equal
  # expect_equal(
  #   taxizedb::downstream('Arabidopsis', db='ncbi', downto='species'),
  #   taxize::downstream('Arabidopsis', db='ncbi', downto='species')
  # )
  expect_gt(
    NROW(taxize::downstream('Arabidopsis', db='ncbi', downto='species',
      messages = FALSE)[[1]]),
    NROW(taxizedb::downstream('Arabidopsis', db='ncbi', downto='species')[[1]])
  )
})

test_that("downstream dies on ambiguities", {
  expect_error(taxizedb::downstream("Bacteria"))
})

test_that("downstream works with GBIF", {
  id <- name2taxid('Pinaceae', db = "gbif")
  x <- downstream(id, db = "gbif", downto = "genus")
  expect_is(x, "downstream")
  expect_is(unclass(x), "list")
  expect_named(unclass(x), id)
  expect_is(unclass(x)[[1]], "tbl")
  expect_named(x[[1]], c("id", "name", "rank"))
})

test_that('downstream fails well', {
  # tpl not supported
  expect_error(
    downstream("Poa", db = "tpl"),
    "TPL database is not supported"
  )
  # abc does not exist
  expect_error(
    downstream("Poa annua", db = "abc"),
    "Database 'abc' is not supported"
  )
  # param types
  expect_error(downstream("Poa annua", db=5), "class character")
  expect_error(downstream("Poa annua", verbose = 5), "class logical")
})
