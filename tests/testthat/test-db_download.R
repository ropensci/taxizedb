context("db_download")

test_that("db_download - are functions", {
  expect_is(db_download_col, "function")
  expect_is(db_download_gbif, "function")
  expect_is(db_download_itis, "function")
  expect_is(db_download_tpl, "function")

  expect_named(formals(db_download_col), "verbose")
  expect_named(formals(db_download_gbif), "verbose")
  expect_named(formals(db_download_itis), "verbose")
  expect_named(formals(db_download_tpl), "verbose")

  expect_named(formals(db_download_tpl), "verbose")
})
