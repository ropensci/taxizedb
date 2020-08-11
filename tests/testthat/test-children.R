context("children")
skip_if_not_installed("taxize")
test_that("unambiguous children", {
  ### TODO: currently there is a type inconsitency in taxize, once that is
  ### cleared up, this test can be restored
  # expect_equal(
  #   taxizedb::children(3701, db='ncbi'),
  #   taxize::children(3701, db='ncbi')
  # )

  ## TODO: these are not currently equal
  ## once changes in taxize are sorted out, we can restore this test
  # expect_equal(
  #   taxizedb::children(2, db='ncbi', ambiguous=FALSE)[[1]],
  #   # need to sqash the walking stick Bacteria genus
  #   taxize::children(2, db='ncbi', ambiguous=FALSE)[[1]] %>%
  #     subset(childtaxa_rank != 'species') %>%
  #     magrittr::set_rownames(NULL)
  # )
})

test_that("ambiguous NCBI children", {
  ## TODO: these are not currently equal
  ## once changes in taxize are sorted out, we can restore this test
  # expect_equal(
  #   taxizedb::children(2, db='ncbi', ambiguous=TRUE)[[1]],
  #   # need to sqash the walking stick Bacteria genus
  #   taxize::children(2, db='ncbi', ambiguous=TRUE)[[1]] %>%
  #     subset(childtaxa_rank != 'species') %>%
  #     magrittr::set_rownames(NULL)
  # )
})

test_that("missing values are consistent with taxize", {
  expect_equal(
    taxizedb::children("asdfasdf", db='ncbi')[[1]],
    taxize::children("asdfasdf", db='ncbi', verbose = FALSE)[[1]]
  )
})
