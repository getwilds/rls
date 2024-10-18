#' Construct a row level security policy
#'
#' @export
#' @importFrom rlang is_empty
#' @param name (character) name of the policy to be created. This must be
#' distinct from the name of any other policy for the table. required
#' @param table (character) the table to apply the policy to. required
#' @param as (character) permissive (default) or restrictive.
#' permissive combines with "OR" while restrictive combines with "AND"
#' @param command (character) the command to which the policy applies. Valid
#' options are ALL (default), SELECT, INSERT, UPDATE, and DELETE
#' @param role (character) The role(s) to which the policy is to be applied.
#' The default is PUBLIC, which will apply the policy to all roles.
#' @param using (character) Specifies a filter that is applied to the WHERE
#' clause of a query. Rows for which the expression returns true will be
#' visible. Any rows for which the expression returns false or null will
#' not be visible to the user (in a SELECT), and will not be available
#' for modification (in an UPDATE or DELETE). Such rows are silently
#' suppressed; no error is reported.
#' @param check (character) the check condition; any SQL conditional expression
#' that returns a boolean. This expression will be used in INSERT and UPDATE
#' queries against the table if row-level security is enabled. Only rows
#' for which the expression evaluates to true will be allowed. Is evaluated
#' against the proposed new contents of the row, not the original contents
#' @references <https://www.postgresql.org/docs/current/sql-createpolicy.html>
#' @return s3 object of class `rls_policy`
#' @details We've chosen more intuitive names for policy parameters, so here's
#' a mapping of function parameters to the PostgreSQL parameters:
#' - (this function: PostgreSQL)
#' - table: on
#' - command: for
#' - role: to
#' - check: with
#' @examples
#' x <- rls_construct_policy(
#'   name = "hide_confidential",
#'   table = "sometable",
#'   check = "confidential BOOLEAN",
#'   using = "confidential = false"
#' )
#' x
rls_construct_policy <- function(
    name, table, as = NULL, command = NULL, role = NULL,
    using = NULL, check = NULL) {
  if (rlang::is_empty(name)) stop("name is a required param")
  if (rlang::is_empty(table)) stop("table is a required param")
  structure(as.list(environment()), class = "rls_policy")
}

#' @export
print.rls_policy <- function(x, ...) {
  compact(x)
  print(glue("<{class(x)}>"))
  print(glue("  policy name: {x$name}"))
  print(glue("  table: {x$table}"))
  print(glue("  as: {x$as}"))
  print(glue("  command: {x$command}"))
  print(glue("  role: {x$role}"))
  print(glue("  using: {x$using}"))
  print(glue("  check: {x$check}"))
}
