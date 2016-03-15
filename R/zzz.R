mssg <- function(v, ...) if (v) message(...)

txdbc <- function(x) Filter(Negate(is.null), x)
