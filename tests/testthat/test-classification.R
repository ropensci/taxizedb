context("classification")

library(taxize, quietly = TRUE, warn.conflicts = FALSE)

# test_that("taxizedb::classification == taxize::classification", {
#   taxa_ids <- c(9606, 3702)
#   taxa_names <- c("Homo sapiens", "Arabidopsis thaliana")
#   taxa_names2 <- c("thale cress", "Homo_sapiens")

#   ## TODO: none of those are equivalent
#   ## slight differences in each
#   expect_equal(
#     taxize::classification(taxa_ids, db='ncbi'),
#     taxizedb::classification(taxa_ids, db='ncbi')
#   )
#   expect_equal(
#     taxize::classification(taxa_names, db='ncbi'),
#     taxizedb::classification(taxa_names, db='ncbi')
#   )
#   # input names are preserved (even if incorrect)
#   expect_equal(
#     taxize::classification(taxa_names2, db='ncbi'),
#     taxizedb::classification(taxa_names2, db='ncbi')
#   )
# })

# test_that("classification is case insensitive", {
#   taxa_names <- c('homo sapiens', 'PIG', 'zea_mays')
#   ## TODO: none of those are equivalent
#   ## slight differences between them
#   # expect_equal(
#   #   taxize::classification(taxa_names, db='ncbi'),
#   #   taxizedb::classification(taxa_names, db='ncbi')
#   # )
# })

test_that('classification handles invalid ids', {
  taxa_ids1 <- 9999999999
  taxa_ids2 <- c(9999999999, 8888888888)
  taxa_ids3 <- c(8888888888, 3702)

  vcr::use_cassette("classification_invalid_ids1", {
    expect_equal(
      taxize::classification(taxa_ids1, db='ncbi'),
      taxizedb::classification(taxa_ids1, db='ncbi')
    )
  }, preserve_exact_body_bytes = FALSE)
  vcr::use_cassette("classification_invalid_ids2", {
    expect_equal(
      taxize::classification(taxa_ids2, db='ncbi'),
      taxizedb::classification(taxa_ids2, db='ncbi')
    )
  }, preserve_exact_body_bytes = FALSE)
  vcr::use_cassette("classification_invalid_ids3", {
    expect_equal(
      taxize::classification(taxa_ids3, db='ncbi'),
      taxizedb::classification(taxa_ids3, db='ncbi')
    )
  }, preserve_exact_body_bytes = FALSE)
})

test_that('classification handles invalid names', {
  taxa_names1 <- 'asdfasdf'
  taxa_names2 <- c('asdfasdf', 'qwerqwer')
  taxa_names3 <- c('pig', 'asdfasdf')
  expect_equal(
    taxize::classification(taxa_names1, db='ncbi'),
    taxizedb::classification(taxa_names1, db='ncbi')
  )
  expect_equal(
    taxize::classification(taxa_names2, db='ncbi'),
    taxizedb::classification(taxa_names2, db='ncbi')
  )
  ## TODO: none of those are equivalent
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
    taxizedb::classification(integer(0), db='ncbi'),
    taxizedb::classification(NULL, db='ncbi')
  )
  expect_equal(
    taxizedb::classification(character(0), db='ncbi'),
    taxizedb::classification(NULL, db='ncbi')
  )
})

test_that('classification handles mixed inputs', {
  x <- c(9606, 'pig', 3702, NA, 'cow', 'zebra', NA)
  lineage <- taxizedb::classification(x, db='ncbi')
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
