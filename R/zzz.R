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
