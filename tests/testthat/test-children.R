context("children")

test_that("unambiguous children", {
  ### TODO: currently there is a type inconsitency in taxize, once that is
  ### cleared up, this test can be restored
  # expect_equal(
  #   taxizedb::children(3702, db='ncbi'),
  #   taxize::children(3702, db='ncbi')
  # )
  vcr::use_cassette("children_unambiguous", {
    expect_equal(
      taxizedb::children(3701, db='ncbi'),
      taxize::children(3701, db='ncbi')
    )
  }, preserve_exact_body_bytes = FALSE)

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

# test_that("ambiguous NCBI children", {
  ## TODO: these are not currently equal
  ## once changes in taxize are sorted out, we can restore this test
  # expect_equal(
  #   taxizedb::children(2, db='ncbi', ambiguous=TRUE)[[1]],
  #   # need to sqash the walking stick Bacteria genus
  #   taxize::children(2, db='ncbi', ambiguous=TRUE)[[1]] %>%
  #     subset(childtaxa_rank != 'species') %>%
  #     magrittr::set_rownames(NULL)
  # )
# })

test_that("missing values are consistent with taxize", {
  empty_df <- data.frame(
    childtaxa_id   = character(0),
    childtaxa_name = character(0),
    childtaxa_rank = character(0),
    stringsAsFactors=FALSE
  )
  expect_equal(
    taxizedb::children("asdfasdf", db='ncbi')[[1]],
    empty_df
    ## TODO: when taxize is updated, replace hard-coded empty_df with:
    # taxize::children("asdfasdf", db='ncbi')[[1]]
  )
})
