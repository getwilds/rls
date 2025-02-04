#' Perform a query
#'
#' @export
#' @importFrom tibble tibble
#' @param query an s3 object of class `privilege` or `row_policy`, required.
#' if `con` is not supplied, we attempt to get the connection
#' from `query`; if it is not found we try to use a value passed to `con`.
#' @param con DBI connection object, optional, see `query`
#' @return R error upon PostgreSQL or Redshift error; upon success a single
#' row tibble with column "rows_affected" (integer)
#' @details This function is similar in spirit to that of
#' `httr2::req_perform()` in that one can construct or privilege or row
#' policy object just as you build up an `httr2` request, but the request
#' is not run until you use the perform function. However, as an alternative,
#' `rls` has an option to make the end of a pipe chain execute the query -
#' see [auto_pipe()].
#'
#' See examples throughout other functions for examples of usage.
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
