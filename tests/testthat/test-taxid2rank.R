context("taxid2rank")

skip_on_cran()

test_that("taxid2rank", {
  expect_equal(taxid2rank(3702), 'species')
  expect_equal(taxid2rank(rep(3702, 2)), rep('species', 2))
  expect_equal(taxid2rank(99999999, warn=F), NA_character_)
  expect_warning(taxid2rank(99999999))
  expect_equal(
    taxid2rank(c(3702,99999999,1,2759), warn=F),
    c('species', NA, 'no rank', 'superkingdom')
  )
  # check order
  expect_equal(
    taxid2rank(c(99999999,1,3702), warn=F),
    c(NA, 'no rank', 'species')
  )
  expect_equal(taxid2rank(NULL), character(0))
})

