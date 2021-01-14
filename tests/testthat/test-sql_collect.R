context("sql_collect")

skip_on_cran()

library(dplyr, quietly = TRUE, warn.conflicts = FALSE)
library(DBI, quietly = TRUE, warn.conflicts = FALSE)
library(RSQLite, quietly = TRUE, warn.conflicts = FALSE)

test_that("sql_collect works", {
  # src
  # src <- dplyr::src_sqlite("irisdb.sqlite", create = TRUE)
  src <- DBI::dbConnect(RSQLite::SQLite(), "irisdb.sqlite")

  expect_is(src, "SQLiteConnection")
  # expect_is(src, "src_sql")

  ## without pipe
  aa <- sql_collect(src, "select * from iris limit 5")
  expect_is(aa, "tbl_df")
  expect_named(aa, c('sepallength', 'sepalwidth', 'petallength',
                     'petalwidth', 'species'))

  ## with pipe
  bb <- src %>% sql_collect("select * from iris limit 5")

  expect_identical(aa, bb)

  # disconnect
  DBI::dbDisconnect(src)
})
