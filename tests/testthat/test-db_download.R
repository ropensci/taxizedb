context("db_download")
test_that("db_download - are functions", {
  skip_on_cran()
  expect_is(db_download_col, "function")
  expect_is(db_download_gbif, "function")
  expect_is(db_download_itis, "function")
  expect_is(db_download_tpl, "function")

  expect_named(formals(db_download_col), c("verbose", "overwrite"))
  expect_named(formals(db_download_gbif), c("verbose", "overwrite"))
  expect_named(formals(db_download_itis), c("verbose", "overwrite"))
  expect_named(formals(db_download_tpl), c("verbose", "overwrite"))
})

test_that("db_download_tpl()", {
  file <- file.path(tdb_cache$cache_path_get(), 'plantlist.sqlite')
  if (file.exists(file)) {
    # existing file can be imported
    suppressMessages(expect_no_error(db_download_tpl()))
    expect_equal(
      capture_messages(db_download_tpl()),
      "Database already exists, returning old file\n"
    )
    # existing file cannot be updated
    expect_error(db_download_tpl(overwrite = TRUE))
  } else {
    # new file cannot be downloaded
    expect_error(db_download_tpl())
  }
})
