context("src")

test_that("src structure is as expected", {
  expect_is(src_itis, "function")
  expect_is(src_gbif, "function")
  expect_is(src_tpl, "function")
  expect_is(src_col, "function")
})


test_that("src fails well", {
  skip_on_cran()
  skip_on_travis()

  expect_error(src_itis(), "argument \"user\" is missing")
  expect_error(src_col(), "Failed to connect")
  expect_error(src_tpl(), "argument \"user\" is missing")

  expect_error(src_gbif(), "argument \"path\" is missing")
  expect_error(src_gbif("Asdf"), "is not TRUE")
  f <- tempfile()
  expect_error(src_gbif(f), "is not TRUE")
})
