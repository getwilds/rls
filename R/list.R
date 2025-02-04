#' List row level security policies
#'
#' @export
#' @param con a DBI database connection object. required. supports only
#' postgres and redshift connections
#' @return `tbl` with RLS policies
#' @details Only difference between postgres and redshift is they use
#' different table names for RLS policies:
#' - Postgres: pg_policies
#' - Redshift: svv_rls_policy
#' @examplesIf has_postgres()
#' library(DBI)
#' library(RPostgres)
#' con <- dbConnect(Postgres())
#' dbWriteTable(con, "attitude", attitude, overwrite = TRUE)
#' rls_tbl(con, "attitude") %>%
#'   row_policy("all_view") %>%
#'   commands(select) %>%
#'   rows_existing(TRUE) %>%
#'   rls_perform()
#' rls_policies(con)
#' rls_drop_policies(con)
#' dbRemoveTable(con, "attitude")
#' dbDisconnect(con)
rls_policies <- function(con) {
  is_conn(con)
  policy_table <- switch(class(con),
    RedshiftConnection = "svv_rls_policy",
    PqConnection = "pg_policies"
  )
  as_tibble(dbGetQuery(con, glue_safe("SELECT * FROM {policy_table}")))
}
