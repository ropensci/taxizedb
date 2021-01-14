context("src")

skip_on_cran()

test_that("src structure is as expected", {
  expect_is(src_itis, "function")
  expect_is(src_gbif, "function")
  expect_is(src_tpl, "function")
  expect_is(src_col, "function")
  expect_is(src_ncbi, "function")
  expect_is(src_wikidata, "function")
  expect_is(src_wfo, "function")
})

test_that("basic functionality of src_* fxns works", {
  x <- src_itis("irisdb.sqlite")
  expect_is(x, "src")
  expect_is(x, "src_dbi")
  expect_is(x, "src_sql")
  expect_match(x$con@dbname, "irisdb")
})

test_that("src fails well", {
  skip_on_cran()
  skip_on_travis()

  # non-existent file
  expect_error(src_itis('foobar'), "not TRUE", class = "error")
  expect_error(src_gbif('foobar'), "not TRUE", class = "error")
  expect_error(src_tpl('foobar'), "not TRUE", class = "error")
  expect_error(src_col('foobar'), "not TRUE", class = "error")
  expect_error(src_ncbi('foobar'), "not TRUE", class = "error")
  expect_error(src_wikidata('foobar'), "not TRUE", class = "error")
  expect_error(src_wfo('foobar'), "not TRUE", class = "error")

  # file exists, but is not a database
  expect_warning(src_col(system.file("CITATION")),
    "file is not a database")
})
