#' As row policy
#' @param x some input
#' @export
#' @return an object of S3 class "row_policy"
as_row_policy <- function(x) {
  UseMethod("as_row_policy")
}
#' @export
as_row_policy.row_policy <- function(x) {
  return(x)
}
#' @export
as_row_policy.tbl_sql <- function(x) {
  tmp <- list(
    data = x,
    name = NULL,
    as = NULL,
    commands = NULL,
    user = NULL,
    existing_rows = NULL,
    new_rows = NULL,
    type = NULL
  )
  structure(tmp, class = "row_policy")
}
#' @export
print.row_policy <- function(x, ...) {
  cat_line(glue("<row_policy> {x$name}"))
  cat_me("as", x$as %||% "PERMISSIVE")
  if (!is_really_empty(x$user)) {
    cat_me("user", x$user)
  }
  if (!is_really_empty(x$commands)) {
    cat_me("commands", x$commands)
  }
  if (!is_really_empty(x$existing_rows)) {
    cat_me("existing rows", x$existing_rows)
  }
  if (!is_really_empty(x$new_rows)) {
    cat_me("new rows", x$new_rows)
  }
  if (!is_really_empty(x$privilege)) {
    cat_me("type", x$type)
    for (i in x$privilege) {
      cat_me(x = i$commands, y = i$cols %|||% "<all cols>", indent = "    ")
    }
  }
  print(x$data)
}
