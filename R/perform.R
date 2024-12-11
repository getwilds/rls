#' Perform a query
#'
#' @export
#' @importFrom tibble tibble
#' @param query an s3 object of class `privilege` or `row_policy, required.
#' if `con` is not supplied, we attempt to get the connection
#' from `query`; if it is not found we try to use a value passed to `con`.
#' @param con DBI connection object, optional, see `query`
#' @return error from PostgreSQL or Redshift upon error; upon success a single
#' row tibble with column "rows_affected" (integer)
rls_perform <- function(query, con = NULL) {
  assert_is(query, c("privilege", "row_policy"))
  con <- as_con(query %||% con)
  is_conn(con)
  sql <- switch(class(query),
    privilege = translate_privilege(query, con),
    row_policy = translate_row_policy(query, con)
  )
  res <- dbExecute(con, sql)
  tibble(rows_affected = res)
}
