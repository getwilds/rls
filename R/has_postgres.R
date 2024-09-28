#' has postgres?
#'
#' @export
#' @param ... further args passed on to [DBI::dbConnect()]
#' @return boolean, `TRUE` if Postgres is app can can be connected to it,
#' `FALSE` if not
has_postgres <- function(...) {
  tryCatch(
    {
      DBI::dbConnect(RPostgres::Postgres(), ...)
      TRUE
    },
    error = function(e) FALSE
  )
}
