context("SQL safety")

skip_on_cran()

test_that("Quotes are handled", {
  # without escapes, the "cow', 'pig" string is interpreted a two values
  expect_equivalent(name2taxid("cow', 'pig"), NA_character_)
  expect_equivalent(length(classification("cow', 'pig")), 1)
  expect_equivalent(length(downstream("cow', 'pig")), 1)
  expect_equivalent(length(children("cow', 'pig")), 1)
})

test_that("taxid-based functions fail on non-integers", {
  expect_error(taxid2name("cow"))
})
