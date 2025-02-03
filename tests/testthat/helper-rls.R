postgres_default <- function(...) {
  tryCatch({
    # replaces RPostgres:::connect_default
    DBI::dbConnect(RPostgres::Postgres(), ...)
  }, error = function(...) {
    vars <- "try setting env vars: PGHOST, PGPORT, PGUSER, PGPASSWORD, PGDATABASE"
    testthat::skip(glue("Test database not available. {vars}"))
  })
}

#' Execute an R expression with access to a database connection.
#'
#' @details Copied unchanged from the RPostgres package, thank you!
#' @param expr (expression) Any R expression.
#' @param con (PqConnection) A database connection, by default.
#' [dbConnect(RPostgres::Postgres())].
#' @return the return value of the evaluated `expr`
with_database_connection <- function(expr, con = postgres_default()) {
  context <- list2env(list(con = con), parent = parent.frame())
  eval(substitute(expr), envir = context)
}
