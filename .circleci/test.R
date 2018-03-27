
Sys.setenv("taxizedb_docker" = "test")
devtools::install()
library("testthat")
## cannot catch errors with test_file()
source("tests/testthat/test-realthing.R")

