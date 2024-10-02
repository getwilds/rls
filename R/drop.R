#' Drop a row level security policy
#'
#' @export
#' @importFrom glue glue
#' @importFrom DBI dbExecute
#' @inheritParams rls_create_policy
#' @param name (character) a policy name. optional
#' @param table (character) a table name. optional
#' @details If `policy` is supplied, `name` and `table` are not required. If
#' `policy` is not supplied, `name` and `table` need to be supplied.
#' @return a scalar numeric that specifies the number of rows affected
#' by the statement, invisibly
#' @references <https://www.postgresql.org/docs/current/sql-droppolicy.html>
#' @examplesIf interactive() && has_postgres()
#' library(DBI)
#' library(RPostgres)
#' con <- dbConnect(Postgres())
#'
#' dbCreateTable(con, "atable", mtcars)
#'
#' policy1 <- rls_construct_policy(
#'   name = "hide_confidential",
#'   on = "atable",
#'   using = "(true)"
#' )
#' policy1
#' rls_create_policy(con, policy1)
#' rls_policies(con)
#' rls_drop_policy(con, policy1)
#' rls_policies(con)
#'
#' dbDisconnect(con)
rls_drop_policy <- function(con, policy = NULL, name = NULL, table = NULL) {
  is_conn(con)
  drop_statement <- switch(class(con),
    RedshiftConnection = "DROP RLS",
    PqConnection = "DROP"
  )
  if (!is.null(policy)) {
    name <- policy$name
    table <- policy$on
  } else {
    if (is.null(name) && is.null(table)) {
      rlang::abort("if `policy` is NULL, name & table must be non-NULL")
    }
  }
  invisible(dbExecute(con, glue("{drop_statement} POLICY {name} ON {table}")))
}
