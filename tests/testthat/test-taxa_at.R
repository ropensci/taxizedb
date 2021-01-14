skip_on_cran()
test_that("taxa_at", {
  ncbi_1 <- taxa_at(186803, rank = "order", db="ncbi", missing = "lower")
  ncbi_2 <- taxa_at(c(186803, 541000, 216572, 186804, 31979,  186806),
    rank = "family", missing = "lower")
  itis <- taxa_at(c(154395, 154357, 23041, 154396), rank = "family", db="itis")
  wfo_1 <- taxa_at(c('wfo-4000032377', 'wfo-0000541830'), rank = "family", db="wfo")
  wfo_2 <- taxa_at("wfo-7000000057", rank = "order", db="wfo")
  gbif_1 <- taxa_at(2877951, rank = "phylum", db="gbif")
  gbif_2 <- taxa_at(c(2877951, 5386), rank = "family", db="gbif")

  for (i in list(ncbi_1, ncbi_2, itis, wfo_1, wfo_2, gbif_1, gbif_2)) expect_is(i, "taxa_at")
  for (i in list(ncbi_1, ncbi_2, itis, wfo_1, wfo_2, gbif_1, gbif_2)) expect_is(unclass(i), "list")
  for (i in list(ncbi_1, ncbi_2, itis, wfo_1, wfo_2, gbif_1, gbif_2)) expect_is(i[[1]], "data.frame")
  for (i in list(ncbi_1, ncbi_2, itis, wfo_1, wfo_2, gbif_1, gbif_2)) expect_named(i[[1]], c("name", "rank", "id"))
  
  skip_on_ci()
  col <- taxa_at(c(3960765, 3953606, 3953010), rank = "family", db="col")
  expect_is(col, "taxa_at")
  expect_is(unclass(col), "list")
  expect_is(col[[1]], "data.frame")
  expect_named(col[[1]], c("name", "rank", "id"))
})
