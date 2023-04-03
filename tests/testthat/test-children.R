context("children")
skip_on_cran()
skip_if_not_installed("taxize")
test_that("NCBI specific: unambiguous children", {
  ### TODO: currently there is a type inconsitency in taxize, once that is
  ### cleared up, this test can be restored
  # expect_equal(
  #   taxizedb::children(3701, db='ncbi'),
  #   taxize::children(3701, db='ncbi')
  # )

  ## TODO: these are not currently equal
  ## once changes in taxize are sorted out, we can restore this test
  # expect_equal(
  #   taxizedb::children(2, db='ncbi', ambiguous=FALSE)[[1]],
  #   # need to sqash the walking stick Bacteria genus
  #   taxize::children(2, db='ncbi', ambiguous=FALSE)[[1]] %>%
  #     subset(childtaxa_rank != 'species') %>%
  #     magrittr::set_rownames(NULL)
  # )
})

test_that("NCBI specific: ambiguous NCBI children", {
  ## TODO: these are not currently equal
  ## once changes in taxize are sorted out, we can restore this test
  # expect_equal(
  #   taxizedb::children(2, db='ncbi', ambiguous=TRUE)[[1]],
  #   # need to sqash the walking stick Bacteria genus
  #   taxize::children(2, db='ncbi', ambiguous=TRUE)[[1]] %>%
  #     subset(childtaxa_rank != 'species') %>%
  #     magrittr::set_rownames(NULL)
  # )
})

test_that("children works for ITIS", {
  res <- children(c(154395, 154357), db="itis")
  expect_is(res, "children")
  expect_is(unclass(res), "list")
  expect_is(res[[1]], "tbl")
  expect_named(res[[1]], c("id", "rank_id", "name", "rank"))
  expect_match(res[[1]]$name, "Apis")
  expect_equal(unique(res[[1]]$rank), "species")
})

test_that("children works for GBIF", {
  res <- children(2877951, db="gbif")
  expect_is(res, "children")
  expect_is(unclass(res), "list")
  expect_is(res[[1]], "tbl")
  expect_named(res[[1]], c("id", "name", "rank"))
  expect_equal(unique(res[[1]]$rank),
    c("species", "variety", "subspecies", "form", "unranked"))
})

test_that("missing values are consistent with taxize", {
  # TODO fix warning instead of suppressing it. This requires fixing deprecated
  # use of select() so it may be best to update this across the whole package.
  df1 <- suppressWarnings(taxizedb::children("asdfasdf", db='ncbi')[[1]])
  df2 <- taxize::children("asdfasdf", db='ncbi', verbose = FALSE)[[1]]
  names(df1) <- names(df2)
  expect_equal(df1, df2)
})

test_that('children fails well', {
  # tpl not supported
  expect_error(
    children("Poa", db = "tpl"),
    "TPL database is not supported"
  )
  # abc does not exist
  expect_error(
    children("Poa annua", db = "abc"),
    "Database 'abc' is not supported"
  )
  # param types
  expect_error(children("Poa annua", db=5), "class character")
  expect_error(children("Poa annua", verbose = 5), "class logical")
})
