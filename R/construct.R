#' Construct an RLS policy
#'
#' @export
#' @param name (character) the policy name. required
#' @param as (character) permissive (default) or restrictive.
#' permissive combines with "OR" while restrictive combines with "AND"
#' @param with (character) xxxx
#' @param using (character) Specifies a filter that is applied to the WHERE
#' clause of a query
#' @param on (character) the table to apply the policy to. default: "all"
#' tables
#' @param to (character) xx
#' @return s3 object of class `rls_policy_redshift`
#' @examples
#' x <- rls_construct_policy(
#'   name = "hide_confidential",
#'   with = "confidential BOOLEAN",
#'   using = "confidential = false"
#' )
#' x
rls_construct_policy <- function(
    name, as = "permissive", with = NULL,
    using = NULL, on = "all", to = "public") {

  query <- as.list(environment())
  structure(query, class = "rls_policy_redshift")
}

#' @export
print.rls_policy_redshift <- function(x, ...) {
  print(glue("<{class(x)}>"))
  print(glue("  policy name: {x$name}"))
  print(glue("  as: {x$as}"))
  print(glue("  with: {x$with}"))
  print(glue("  using: {x$using}"))
  print(glue("  on: {x$on}"))
  print(glue("  to: {x$to}"))
}
