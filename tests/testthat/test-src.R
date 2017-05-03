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

  expect_error(src_itis(), "could not connect")
  expect_error(src_col(), "Failed to connect")
  expect_error(src_tpl(), "could not connect")

  expect_error(src_gbif(), "argument \"path\" is missing")
  expect_error(src_gbif("Asdf"), "Path does not exist")
  f <- tempfile()
  expect_error(src_gbif(f), "Path does not exist")
})
