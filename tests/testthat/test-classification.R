context("classification")
skip_on_cran()
skip_if_not_installed("taxize")
library(taxize, quietly = TRUE, warn.conflicts = FALSE)

test_that("taxizedb::classification == taxize::classification", {
  taxa_ids <- c(9606, 3702)
  taxa_names <- c("Homo sapiens", "Arabidopsis thaliana")
  taxa_names2 <- c("thale cress", "Homo_sapiens")

  # FIXME: "clade" (taxizedb dump) vs. "no rank" (taxize)
  # ENTREZ api not using the same data as is in the dump
  # expect_equal(
  #   taxize::classification(taxa_ids, db='ncbi', messages = FALSE),
  #   taxizedb::classification(taxa_ids, db='ncbi', verbose = FALSE)
  # )
  
  # FIXME: "clade" (taxizedb dump) vs. "no rank" (taxize)
  # ENTREZ api not using the same data as is in the dump
  # expect_equal(
  #   taxize::classification(taxa_names, db='ncbi', messages = FALSE),
  #   taxizedb::classification(taxa_names, db='ncbi')
  # )
  
  # FIXME: "clade" (taxizedb dump) vs. "no rank" (taxize)
  # ENTREZ api not using the same data as is in the dump
  # input names are preserved (even if incorrect)
  # expect_equal(
  #   taxize::classification(taxa_names2, db='ncbi', messages = FALSE),
  #   taxizedb::classification(taxa_names2, db='ncbi')
  # )
})

test_that("classification is case insensitive", {
  taxa_names <- c('homo sapiens', 'PIG', 'zea_mays')
  ## FIXME: none of those are equivalent
  ## slight differences between them
  # FIXME: "clade" (taxizedb dump) vs. "no rank" (taxize)
  # ENTREZ api not using the same data as is in the dump
  # expect_equal(
  #   taxize::classification(taxa_names, db='ncbi', messages = FALSE),
  #   taxizedb::classification(taxa_names, db='ncbi')
  # )
})

test_that('classification handles invalid ids', {
  # FIXME: unfortunately, now taxize gives character(0) for classification with ncbi
  # taxa_ids1 <- 9999999999
  # taxa_ids2 <- c(9999999999, 8888888888)
  # taxa_ids3 <- c(8888888888, 3702)
  # expect_equal(
  #   taxize::classification(taxa_ids1, db='ncbi', messages = FALSE),
  #   taxizedb::classification(taxa_ids1, db='ncbi', verbose = FALSE)
  # )
  # expect_equal(
  #   taxize::classification(taxa_ids2, db='ncbi', messages = FALSE),
  #   taxizedb::classification(taxa_ids2, db='ncbi', verbose = FALSE)
  # )
  # expect_equal(
  #   taxize::classification(taxa_ids3, db='ncbi', messages = FALSE),
  #   taxizedb::classification(taxa_ids3, db='ncbi', verbose = FALSE)
  # )
})

test_that('classification handles invalid names', {
  taxa_names1 <- 'asdfasdf'
  taxa_names2 <- c('asdfasdf', 'qwerqwer')
  taxa_names3 <- c('pig', 'asdfasdf')
  expect_equal(
    suppressWarnings(taxize::classification(taxa_names1, db='ncbi',
      messages = FALSE)),
    taxizedb::classification(taxa_names1, db='ncbi', verbose = FALSE)
  )
  expect_equal(
    suppressWarnings(taxize::classification(taxa_names2, db='ncbi',
      messages = FALSE)),
    taxizedb::classification(taxa_names2, db='ncbi', verbose = FALSE)
  )
  ## FIXME: none of those are equivalent
  ## slight differences between them
  # expect_equal(
  #   taxize::classification(taxa_names3, db='ncbi'),
  #   taxizedb::classification(taxa_names3, db='ncbi')
  # )
})

test_that('classification(NULL) == list()', {
  expect_equal(
    taxizedb::classification(NULL, db='ncbi'),
    {
      x <- list()
      attributes(x) <- list(names=names(x), class='classification', db='ncbi')
      x
    }
  )
  expect_equal(
    taxizedb::classification(integer(0), db='ncbi', verbose = FALSE),
    taxizedb::classification(NULL, db='ncbi', verbose = FALSE)
  )
  expect_equal(
    taxizedb::classification(character(0), db='ncbi', verbose = FALSE),
    taxizedb::classification(NULL, db='ncbi', verbose = FALSE)
  )
})

test_that('classification handles mixed inputs', {
  x <- c(9606, 'pig', 3702, NA, 'cow', 'zebra', NA)
  lineage <- taxizedb::classification(x, db='ncbi', verbose = FALSE)
  expect_equal(
    names(lineage),
    c('9606', 'pig', '3702', NA, 'cow', 'zebra', NA)
  )
  expect_equal(
    unname(sapply(lineage, class)[c(1:3, 5)]),
    rep('data.frame', 4)
  )
  expect_equal(unname(which(is.na(lineage))), c(4,6,7))
})

test_that('classification fails well', {
  # tpl not supported
  expect_error(
    classification("Poa annua", db = "tpl"),
    "TPL database is not supported"
  )
  # abc does not exist
  expect_error(
    classification("Poa annua", db = "abc"),
    "Database 'abc' is not supported"
  )
  # param types
  expect_error(classification("Poa annua", db=5), "class character")
  expect_error(classification("Poa annua", verbose = 5), "class logical")
})
