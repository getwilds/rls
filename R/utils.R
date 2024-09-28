is_conn <- function(con) {
  stopifnot(!inherits(con, "DBIConnection") ==
    "con must be of class DBIConnection")
}

compact <- function(x) {
  Filter(Negate(is.null), x)
}
