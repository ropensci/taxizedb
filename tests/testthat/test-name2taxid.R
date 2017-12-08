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

test_that("name2taxid works for ambiguous cases", {
  tax_names <- c('pig', 'Bacteria', 'horse')
  expected_df <- tibble::data_frame(
    name_txt = c("pig", "Bacteria", "Bacteria", "horse"),
    tax_id = c(9823L, 2L, 629395L, 9796L)
  )
  expect_equal(name2taxid(tax_names, out_type='summary'), expected_df, out_type='summary') 
})
