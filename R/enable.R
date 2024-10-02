#' Enable row level security on a table
#'
#' @export
#' @importFrom DBI dbExecute
#' @param con a DBI database connection object. required. supports only
#' postgres and redshift connections
#' @param table (character) a table name. required
#' @return a scalar numeric that specifies the number of rows affected
#' by the statement, invisibly
#' @examplesIf has_postgres()
#' library(DBI)
#' library(RPostgres)
#' con <- dbConnect(Postgres())
#' dbListTables(con)
#' dbWriteTable(con, "mtcars", mtcars, temporary = TRUE)
#' rls_enable(con, table = "mtcars")
#' rls_check_status(con, "mtcars")
#' rls_disable(con, table = "mtcars")
#' rls_check_status(con, "mtcars")
#' dbRemoveTable(con, "mtcars")
#' dbDisconnect(con)
rls_enable <- function(con, table) {
  is_conn(con)
  enable_chunk <- switch(class(con),
    RedshiftConnection = "ROW LEVEL SECURITY ON",
    PqConnection = "ENABLE ROW LEVEL SECURITY"
  )
  invisible(dbExecute(con, glue_safe("ALTER TABLE {table} {enable_chunk}")))
}

#' @export
#' @rdname rls_enable
rls_disable <- function(con, table) {
  is_conn(con)
  enable_chunk <- switch(class(con),
    RedshiftConnection = "ROW LEVEL SECURITY OFF",
    PqConnection = "DISABLE ROW LEVEL SECURITY"
  )
  invisible(dbExecute(con, glue_safe("ALTER TABLE {table} {enable_chunk}")))
}
