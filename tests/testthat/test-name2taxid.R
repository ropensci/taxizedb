context("name2taxid")

test_that("name2taxid", {
  expect_equal(name2taxid('Arabidopsis thaliana'), "3702")
  expect_equal(name2taxid('Sparkly unicorn'), NA_character_)
  expect_equal(
    name2taxid(c('Arabidopsis thaliana', "Gingerbread man", 'root')),
    as.character(c(3702,NA,1))
  )
  expect_equal(name2taxid(NULL), character(0))
})
