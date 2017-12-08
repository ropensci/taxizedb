context("children")

test_that("children", {
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
})
