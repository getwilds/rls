#' Check if a row level security policy exists
#'
#' @export
#' @param con a DBI database connection object. required. supports only
#' postgres and redshift connections
#' @param name (character) a policy name. required
#' @return scalar boolean, `TRUE` or `FALSE`
#' @examplesIf has_postgres()
#' library(DBI)
#' library(RPostgres)
#' con <- dbConnect(Postgres())
#' dbWriteTable(con, "attitude", attitude, overwrite = TRUE)
#' dbExecute(con, "DROP ROLE IF EXISTS jane")
#' dbExecute(con, "CREATE ROLE jane")
#' policy <- rls_tbl(con, "attitude") %>%
#'   row_policy(name = "some_policy") %>%
#'   commands(update) %>% 
#'   rows_existing(TRUE) %>%
#'   to(jane)
#' policy
#' rls_run(policy)
#' rls_policies(con)
#' rls_policy_exists(con, "some_policy")
#' rls_drop_policy(con, name = "some_policy", table = "attitude")
#' rls_policy_exists(con, "some_policy")
#' dbRemoveTable(con, "attitude")
#' dbExecute(con, "DROP ROLE jane")
#' dbDisconnect(con)
rls_policy_exists <- function(con, name) {
  is_conn(con)
  pols <- rls_policies(con)
  if (NROW(pols) == 0) {
    return(FALSE)
  }
  name %in% pols$policyname
}
