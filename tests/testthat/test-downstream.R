context("downstream")

test_that("taxizedb::downstream matches taxize::downstream", {
  expect_equal(
    taxizedb::downstream('Arabidopsis', db='ncbi', downto='species'),
    taxize::downstream('Arabidopsis', db='ncbi', downto='species')
  )
})
