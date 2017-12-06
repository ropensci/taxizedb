context("name2taxid")

test_that("name2taxid", {
  expect_equal(taxid2name(3702), 'Arabidopsis thaliana')
  expect_equal(taxid2name(99999999), NA_character_)
  expect_equal(
    taxid2name(c(3702,99999999,1)),
    c('Arabidopsis thaliana', NA, 'root')
  )
  expect_equal(taxid2name(NULL), character(0))
})
