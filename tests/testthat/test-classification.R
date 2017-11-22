context("classification")

library(taxize, quietly = TRUE, warn.conflicts = FALSE)

test_that("taxizedb::classification == taxize::classification", {
  taxa <- c(3702, 9606)
  expect_equal(
    taxize::classification(taxa, db='ncbi'),
    taxizedb::classification(taxa, db='ncbi')
  )
})
