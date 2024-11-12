is_conn <- function(con) {
  stopifnot(
    "con must be of class DBIConnection" =
    inherits(con, "DBIConnection")
  )
}

compact <- function(x) {
  Filter(Negate(is.null), x)
}

#' @importFrom cli ansi_collapse
clz_col <- function(x) {
  ansi_collapse(x, sep2 = " or ", last = ", or ")
}

#' @importFrom cli cli_abort
#' @importFrom rlang caller_env
rls_abort <- function(message, call = caller_env(2)) {
  cli_abort(message, call = call)
}

#' @importFrom cli format_error
#' @importFrom rlang abort caller_arg
assert_is <- function(x, y, arg = caller_arg(x)) {
  if (!inherits(x, y)) {
    rls_abort(
      format_error("{.arg {arg}} must be class {.strong {clz_col(y)}}")
    )
  }
}

#' @importFrom rlang is_scalar_character
assert_scalar <- function(x, arg = caller_arg(x)) {
  if (!is_scalar_character(x)) {
    rls_abort(
      format_error("{.arg {arg}} must be scalar")
    )
  }
}

#' @importFrom rlang has_length
assert_len <- function(x, y, arg = caller_arg(x)) {
  if (!has_length(x, y)) {
    rls_abort(format_error("{.arg {arg}} must have length {y}"))
  }
}

trimws_inside <- function(x) {
  gsub("\\s+", " ", x)
}

`%|||%` <- function(x, y) {
  if (
    missing(x) ||
      is.null(x) ||
      all(nchar(x) == 0) ||
      length(x) == 0) {
    y
  } else {
    x
  }
}

is_really_empty <- function(x, y) {
  missing(x) ||
    is.null(x) ||
    all(nchar(x) == 0) ||
    length(x) == 0
}

dot_names <- function(...) {
  vapply(enquos(...), as_name, "")
}

collapse <- function(x) {
  paste(x, collapse = ", ")
}
