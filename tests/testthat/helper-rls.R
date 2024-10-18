#' Execute an R expression with access to a database connection.
#'
#' @details Copied from the RPostgres package, thank you!
#' @param expr (expression) Any R expression.
#' @param con (PqConnection) A database connection, by default.
#' [dbConnect(RPostgres::Postgres())].
#' @return the return value of the evaluated `expr`
with_database_connection <- function(expr, con = RPostgres::postgresDefault()) {
  context <- list2env(list(con = con), parent = parent.frame())
  eval(substitute(expr), envir = context)
}
