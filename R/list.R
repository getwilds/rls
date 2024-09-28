#' List row level security policies
#'
#' @export
#' @importFrom DBI dbGetQuery
#' @importFrom RPostgres Postgres
#' @importFrom tibble as_tibble
#' @importFrom glue glue
#' @param con a DBI database connection object. required. supports only
#' postgres and redshift connections
#' @return tibble with RLS policies
#' @details Only difference between postgres and redshift is they use
#' different table names for RLS policies:
#' - Postgres: pg_policies
#' - Redshift: svv_rls_policy
#' @examplesIf has_postgres()
#' library(DBI)
#' library(RPostgres)
#' con <- dbConnect(Postgres())
#' rls_policies(con)
#' dbDisconnect(con)
rls_policies <- function(con) {
  is_conn(con)
  policy_table <- switch(class(con),
    RedshiftConnection = "svv_rls_policy",
    PqConnection = "pg_policies"
  )
  as_tibble(dbGetQuery(con, glue("SELECT * FROM {policy_table}")))
}
