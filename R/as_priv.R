#' As privilege
#' @param x some input
#' @export
as_priv <- function(x) {
  UseMethod("as_priv")
}
#' @export
as_priv.privilege <- function(x) {
  return(x)
}
#' @export
as_priv.tbl_sql <- function(x) {
  tmp <- list(
    data = x,
    user = NULL,
    privilege = list(),
    type = NULL
  )
  structure(tmp, class = "privilege")
}
#' @export
#' @importFrom cli cat_line
print.privilege <- function(x, ...) {
  cat_line("<privilege>")
  if (!is_really_empty(x$user)) cat_me("user", x$user)
  if (!is_really_empty(x$privilege)) {
    cat_me("type", x$type)
    for (i in x$privilege) {
      cat_me(x=i$commands, y=i$cols %|||% "<all cols>", indent = "    ")
    }
  }
  print(x$data)
}

cat_me <- function(x, y, indent = "  ") {
  y <- paste0(y, collapse = ", ")
  cat_line(glue("{indent}{x}: {y}", .trim = FALSE))
}

rls_grant <- function(commands, cols) {
  x <- list(commands = commands, cols = cols)
  structure(x, class = "rls_grant")
}
rls_revoke <- function(commands, cols) {
  x <- list(commands = commands, cols = cols)
  structure(x, class = "rls_revoke")
}
