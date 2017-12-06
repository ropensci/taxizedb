context("name2taxid")

src <- src_ncbi(db_download_ncbi())

test_that("name2taxid", {
  expect_equal(name2taxid('Arabidopsis thaliana'), "3702")
  expect_equal(name2taxid('Sparkly unicorn'), NA_character_)
  expect_equal(
    name2taxid(c('root', 'Arabidopsis thaliana', "Gingerbread man")),
    as.character(c(1,3702,NA))
  )
  expect_equal(name2taxid(NULL), character(0))
})
