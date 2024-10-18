#' List row level security policies
#'
#' @export
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
#' dbWriteTable(con, "attitude", attitude, temporary = TRUE)
#' my_policy <- rls_construct_policy(
#'   name = "all_view",
#'   table = "attitude",
#'   command = "SELECT",
#'   using = "(true)"
#' )
#' rls_create_policy(con, my_policy)
#' rls_policies(con)
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
