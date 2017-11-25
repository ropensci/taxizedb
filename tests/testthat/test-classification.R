context("classification")

library(taxize, quietly = TRUE, warn.conflicts = FALSE)

test_that("taxizedb::classification == taxize::classification", {
  taxa_ids <- c(3702, 9606)
  taxa_names <- c("Arabidopsis thaliana", "Homo sapiens")
  expect_equal(
    taxize::classification(taxa_ids, db='ncbi'),
    taxizedb::classification(taxa_ids, db='ncbi')
  )
  expect_equal(
    taxize::classification(taxa_names, db='ncbi'),
    taxizedb::classification(taxa_names, db='ncbi')
  )
})
