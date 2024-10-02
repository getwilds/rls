#' Construct a row level security policy
#'
#' @export
#' @importFrom rlang is_empty
#' @param name (character) the policy name. required
#' @param on (character) the table to apply the policy to. required
#' @param as (character) permissive (default) or restrictive.
#' permissive combines with "OR" while restrictive combines with "AND"
#' @param for_ (character) permissive (default) or restrictive.
#' @param to (character) The role(s) to which the policy is to be applied.
#' The default is PUBLIC, which will apply the policy to all roles.
#' @param using (character) Specifies a filter that is applied to the WHERE
#' clause of a query
#' @param with (character) the check condition
#' @references <https://www.postgresql.org/docs/current/sql-createpolicy.html>
#' @return s3 object of class `rls_policy`
#' @examples
#' x <- rls_construct_policy(
#'   name = "hide_confidential",
#'   on = "sometable",
#'   with = "confidential BOOLEAN",
#'   using = "confidential = false"
#' )
#' x
rls_construct_policy <- function(
    name, on, as = NULL, for_ = NULL, to = NULL,
    using = NULL, with = NULL) {
  if (rlang::is_empty(name)) stop("name is a required param")
  if (rlang::is_empty(on)) stop("on is a required param")
  structure(as.list(environment()), class = "rls_policy")
}

#' @export
print.rls_policy <- function(x, ...) {
  compact(x)
  print(glue("<{class(x)}>"))
  print(glue("  policy name: {x$name}"))
  print(glue("  on: {x$on}"))
  print(glue("  as: {x$as}"))
  print(glue("  for: {x$for_}"))
  print(glue("  to: {x$to}"))
  print(glue("  using: {x$using}"))
  print(glue("  with: {x$with}"))
}
