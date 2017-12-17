context("SQL safety")

test_that("Quotes are handled", {
  expect_equivalent(name2taxid("cow', 'pig"), NA_character_)  
})

test_that("taxid-based functions fail on non-integers", {
  expect_error(taxid2name("cow"))
})
