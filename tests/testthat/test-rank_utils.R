test_that("rank_utils", {
  skip_on_cran()
  
  expect_is(txdb_ids, 'character')
  expect_is(txdb_ranks, 'character')
  expect_is(txdb_rr, 'data.frame')
  expect_named(txdb_rr, c("rankid", "ranks"))

  expect_equal(txdb_which_rank("species"), 37)
  expect_equal(unname(txdb_which_rank_v(c("species", "genus"))),
    c(37, 31))
  df <- data.frame(rank = c("family","genus","species"), stuff=1:3)
  expect_equal(NROW(txdb_prune_too_low(df, "genus")), 2)
})
