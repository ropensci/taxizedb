context("classification")

library(taxize, quietly = TRUE, warn.conflicts = FALSE)

test_that("taxizedb::classification == taxize::classification", {
  taxa_ids <- c(9606, 3702)
  taxa_names <- c("Homo sapiens", "Arabidopsis thaliana")
  taxa_names2 <- c("thale cress", "Homo_sapiens")
  expect_equal(
    taxize::classification(taxa_ids, db='ncbi'),
    taxizedb::classification(taxa_ids, db='ncbi')
  )
  expect_equal(
    taxize::classification(taxa_names, db='ncbi'),
    taxizedb::classification(taxa_names, db='ncbi')
  )
  # input names are preserved (even if incorrect)
  expect_equal(
    taxize::classification(taxa_names2, db='ncbi'),
    taxizedb::classification(taxa_names2, db='ncbi')
  )
})
