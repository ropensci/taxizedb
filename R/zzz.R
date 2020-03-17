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

sm <- function(x) suppressMessages(x)
