#' tbl variant for rls
#'
#' @importFrom dplyr tbl filter
#' @importFrom dbplyr sql
#' @export
#' @param con a postgres or redshift connection object
#' @param from (character) a table name
#' @param ... args passed on to [dplyr::tbl()]
#' @autoglobal
#' @return a `tbl`
#' @examplesIf has_postgres()
#' library(DBI)
#' library(RPostgres)
#' con <- dbConnect(Postgres())
#' setup_example_table(con)
#' user <- rls_current_user(con)
#' dbExecute(con, "CREATE ROLE ally")
#' # rls_tbl(con, "passwd") # should fail
#' rls_tbl(con, "passwd") %>%
#'   grant(select) %>%
#'   to(ally) %>%
#'   rls_perform()
#' dbExecute(con, "SET SESSION AUTHORIZATION ally")
#' rls_tbl(con, "passwd")
#' dbExecute(con, glue::glue("SET SESSION AUTHORIZATION {user}"))
#' rls_tbl(con, "passwd")
#' 
#' # cleanup
#' dbRemoveTable(con, "passwd")
#' dbExecute(con, "DROP ROLE ally")
#' dbDisconnect(con)
rls_tbl <- function(con, from, ...) {
  privs <- rls_column_privileges(con, from, rls_current_user(con))
  sql_custom <- if (NROW(privs) == 0) {
    sql(glue("SELECT * FROM {from}"))
  } else {
    privs <- dplyr::filter(privs, privilege_type == "SELECT")
    sql(sprintf("SELECT %s FROM %s", collapse(privs$column_name), from))
  }
  our_tbl <- tbl(con, sql_custom, ...)
  attr(our_tbl, "table") <- from
  our_tbl
}
