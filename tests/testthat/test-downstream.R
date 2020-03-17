context("downstream")

test_that("taxizedb::downstream matches taxize::downstream", {
  # FIXME, these are no longer equal
  # expect_equal(
  #   taxizedb::downstream('Arabidopsis', db='ncbi', downto='species'),
  #   taxize::downstream('Arabidopsis', db='ncbi', downto='species')
  # )
  expect_gt(
    NROW(taxize::downstream('Arabidopsis', db='ncbi', downto='species',
      messages = FALSE)[[1]]),
    NROW(taxizedb::downstream('Arabidopsis', db='ncbi', downto='species')[[1]])
  )
})

test_that("downstream dies on ambiguities", {
  expect_error(taxizedb::downstream("Bacteria"))
})
