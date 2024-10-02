#' Check row level security status of a table
#'
#' @export
#' @inheritParams rls_enable
#' @return tibble with columns:
#' - relname
#' - relrowsecurity
#' - relforcerowsecurity
rls_check_status <- function(con, table) {
  query <- glue_safe("select relname, relrowsecurity, relforcerowsecurity
		from pg_class
		where oid = '{table}'::regclass")
  as_tibble(dbGetQuery(con, query))
}
