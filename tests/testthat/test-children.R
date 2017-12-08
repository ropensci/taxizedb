context("children")

test_that("unambiguous children", {
  ### TODO: currently there is a type inconsitency in taxize, once that is
  ### cleared up, this test can be restored
  # expect_equal(
  #   taxizedb::children(3702, db='ncbi'),
  #   taxize::children(3702, db='ncbi')
  # )
  expect_equal(
    taxizedb::children(3701, db='ncbi'),
    taxize::children(3701, db='ncbi')
  )
  expect_equal(
    taxizedb::children(2, db='ncbi', ambiguous=FALSE)[[1]],
    # need to sqash the walking stick Bacteria genus
    taxize::children(2, db='ncbi', ambiguous=FALSE)[[1]] %>%
      subset(childtaxa_rank != 'species') %>%
      set_rownames(NULL)
  )
})

test_that("ambiguous NCBI children", {
  expect_equal(
    taxizedb::children(2, db='ncbi', ambiguous=TRUE)[[1]],
    # need to sqash the walking stick Bacteria genus
    taxize::children(2, db='ncbi', ambiguous=TRUE)[[1]] %>%
      subset(childtaxa_rank != 'species') %>%
      set_rownames(NULL)
  )
})
