context("name2taxid")

skip_on_cran()
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
  expected_df <- tibble::tibble(
    name = c("pig", "Bacteria", "Bacteria", "horse"),
    id = c("9823", "2", "629395", "9796")
  )
  expect_equal(name2taxid(tax_names, out_type='summary'), expected_df,
    out_type='summary')
})

test_that("name2taxid correctly handles space/underscore", {
  # "Homo_sapiens" is not in the SQL names table, but "Homo sapiens" is.
  # taxizedb will replace the underscore with a space.
  expect_equal(name2taxid("Homo_sapiens"), name2taxid("Homo sapiens"))
  # Here the underscore is an not just a standin for a space. taxizedb replaces
  # underscores with spaces EXCEPT when a space is already present in the name:
  # FIXME: this used to work
  # expect_equal(name2taxid("haloarchaeon 3A1_DGR"), "1071085")
})

test_that("name2taxid(out_type='uid') dies if ambiguous", {
  expect_error(name2taxid("Bacteria", out_type='uid'))
})

test_that("name2taxid works with duplicates", {
  expect_equal(name2taxid(c("Arabidopsis", "Arabidopsis"), out_type='uid'),
    c("3701", "3701"))
})

test_that("name2taxid summary works", {
  expect_equal(
    name2taxid('dragon', out_type='summary'),
    tibble::tibble(name=character(), id=character())
  )
})
