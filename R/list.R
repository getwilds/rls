#' List RLS policies
#'
#' @export
#' @importFrom DBI dbGetQuery
#' @importFrom tibble as_tibble
#' @param con a DBI database connection object. required
#' @return tibble with RLS policies
#' @examplesIf interactive()
#' rls_policies(con)
rls_policies <- function(con) {
  sql <- "SELECT * FROM svv_rls_policy"
  as_tibble(dbGetQuery(con, sql))
}
