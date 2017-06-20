mssg <- function(v, ...) if (v) message(...)

txdbc <- function(x) Filter(Negate(is.null), x)

cl <- function(x, y){
  if (is.null(y)) {
    ""
  } else {
    paste0(x, y)
  }
}

mkhome <- function(x) {
  dir.create(x, showWarnings = FALSE, recursive = FALSE)
}

# db_installed(x = 'psql')
db_installed <- function(x) {
  tmp <- Sys.which(x)
  if (any(tmp == "")) {
    nf <- paste0(names(tmp)[tmp == ""], collapse = ", ")
    stop(
      sprintf(
  "\n%s not found on your computer\nInstall the missing tool(s) and try again",
  nf))
  }
}

# db_on(x = 'psql')
# db_on <- function(x) {
#   tmp <- system("ps aux | grep postgres", intern = TRUE)
#   tmp
#   if (any(tmp == "")) {
#     nf <- paste0(names(tmp)[tmp == ""], collapse = ", ")
#     stop(sprintf("\n%s not turned on\nPlease turn it on", nf))
#   }
# }
