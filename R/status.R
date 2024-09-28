#' Check row level security status of a table
#'
#' @export
#' @inheritParams rls_enable
#' @return tibble
rls_check_status <- function(con, table) {
  query <- glue("select relname, relrowsecurity, relforcerowsecurity
		from pg_class
		where oid = '{table}'::regclass")
  dbGetQuery(con, query)
}
