context("taxid2name")

skip_on_cran()

test_that("taxid2name", {
  expect_equal(taxid2name(3702), 'Arabidopsis thaliana')
  # check duplicates
  expect_equal(taxid2name(c(3702, 3702)), rep('Arabidopsis thaliana', 2))
  expect_equal(taxid2name(99999999, warn=F), NA_character_)
  expect_warning(taxid2name(99999999))
  expect_equal(
    taxid2name(c(3702,99999999,1), warn=F),
    c('Arabidopsis thaliana', NA, 'root')
  )
  # check order
  expect_equal(
    taxid2name(c(99999999,1,3702), warn=F),
    c(NA, 'root', 'Arabidopsis thaliana')
  )
  expect_equal(taxid2name(NULL), character(0))
})
