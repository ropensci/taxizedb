context("db_load")

test_that("db_load fails as expected", {
  skip_on_cran()

  # parameter errors
  ## path
  expect_error(suppressMessages(db_load_col(path = "asdfasdf")),
               "file.exists\\(path\\) is not TRUE")
  expect_error(suppressMessages(db_load_itis(path = "asdfasdf")),
               "file.exists\\(path\\) is not TRUE")
  expect_error(suppressMessages(db_load_tpl(path = "asdfasdf")),
               "file.exists\\(path\\) is not TRUE")

  ## user missing
  f <- tempfile()
  cat("hello world", file = f)

  expect_error(suppressMessages(db_load_itis(path = f)),
               "argument \"user\" is missing, with no default")
  expect_error(suppressMessages(db_load_tpl(path = f)),
               "argument \"user\" is missing, with no default")
})

test_that("db_load fails as expected - more", {
  skip_on_cran()
  skip_on_travis()

  f <- tempfile()
  cat("hello world", file = f)

  # connection failures
  expect_error(suppressMessages(db_load_col(path = f)), "Failed to connect")
  expect_error(suppressMessages(db_load_itis(path = f, user = "stuff")),
               "Make sure Postgres is on/running")
  expect_error(suppressMessages(db_load_tpl(path = f, user = "stuff")),
               "Make sure Postgres is on/running")

  # doesn't fail, is just checking that sqlite installed
  expect_message(db_load_gbif(), "checking if SQLite installed")
})
