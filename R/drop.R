#' Drop row level security policies
#'
#' @export
#' @importFrom glue glue
#' @importFrom DBI dbExecute
#' @inheritParams rls_create_policy
#' @param name (character) a policy name. optional
#' @param table (character) a table name. optional
#' @details If `policy` is supplied, `name` and `table` are not required. If
#' `policy` is not supplied, `name` and `table` need to be supplied.
#' @return For `rls_drop_policy`, a scalar numeric that specifies the number
#' of rows affected by the statement, invisibly. For `rls_drop_policies`,
#' return `NULL` invisibly
#' @references <https://www.postgresql.org/docs/current/sql-droppolicy.html>
#' @details `rls_drop_policy` drops a single policy, and `rls_drop_policies`
#' drops all policies in the database defined by the `con`
#' @examplesIf has_postgres()
#' library(DBI)
#' library(RPostgres)
#' con <- dbConnect(Postgres())
#'
#' dbWriteTable(con, "atable", mtcars, overwrite = TRUE)
#'
#' policy1 <- rls_tbl(con, "atable") %>%
#'   row_policy("hide_confidential") %>%
#'   rows_existing(TRUE)
#' rls_run(policy1)
#'
#' rls_policies(con)
#' rls_drop_policy(con, policy1)
#' rls_policies(con)
#'
#' rls_run(policy1)
#' rls_policies(con)
#' rls_drop_policies(con)
#' rls_policies(con)
#'
#' dbRemoveTable(con, "atable")
#' dbDisconnect(con)
rls_drop_policy <- function(con, policy = NULL, name = NULL, table = NULL) {
  is_conn(con)
  drop_statement <- switch(class(con),
    RedshiftConnection = "DROP RLS",
    PqConnection = "DROP"
  )
  if (!is.null(policy)) {
    name <- policy$name
    table <- attr(policy$data, "table")
  } else {
    if (is.null(name) || is.null(table)) {
      rlang::abort("if `policy` is NULL, `name` & `table` must be non-NULL")
    }
  }
  invisible(dbExecute(con, glue("{drop_statement} POLICY {name} ON {table}")))
}

#' @export
#' @rdname rls_drop_policy
rls_drop_policies <- function(con) {
  pols <- rls_policies(con)
  if (NROW(pols) == 0) return(invisible())
  for (i in seq_len(NROW(pols))) {
    invisible(
      rls_drop_policy(con,
        name = pols$policyname[i],
        table = pols$tablename[i]
      )
    )
  }
}
