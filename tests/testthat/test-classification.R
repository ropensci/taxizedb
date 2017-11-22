context("classification")

library(taxize, quietly = TRUE, warn.conflicts = FALSE)

test_that("taxizedb::classification == taxize::classification", {
  src <- src_ncbi(db_download_ncbi())
  taxa <- c(3702, 9606)
  expect_equal(
    taxize::classification(taxa, db='ncbi'),
    taxizedb::classification(src, taxa, db='ncbi')
  )
})
