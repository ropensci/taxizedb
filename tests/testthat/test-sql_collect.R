context("sql_collect")

library(dplyr, quietly = TRUE, warn.conflicts = FALSE)

test_that("sql_collect works", {
  # src
  src <- dplyr::src_sqlite("irisdb.sqlite", create = TRUE)

  expect_is(src, "src_dbi")
  expect_is(src, "src_sql")

  ## without pipe
  aa <- sql_collect(src, "select * from iris limit 5")
  expect_is(aa, "tbl_df")
  expect_named(aa, c('sepallength', 'sepalwidth', 'petallength',
                     'petalwidth', 'species'))

  ## with pipe
  bb <- src %>% sql_collect("select * from iris limit 5")

  expect_identical(aa, bb)
})
