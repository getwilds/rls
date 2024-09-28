#' Enable row level security on a table
#'
#' @export
#' @importFrom DBI dbExecute
#' @param con a DBI database connection object. required. supports only
#' postgres and redshift connections
#' @param table (character) a table name. required
#' @return NULL
#' @examplesIf has_postgres()
#' library(DBI)
#' library(RPostgres)
#' con <- dbConnect(Postgres())
#' dbListTables(con)
#' rls_enable(con, table = "accounts")
#' rls_check_status(con, "accounts")
#' rls_disable(con, table = "accounts")
#' rls_check_status(con, "accounts")
#' dbDisconnect(con)
rls_enable <- function(con, table) {
  is_conn(con)
  enable_chunk <- switch(class(con),
    RedshiftConnection = "ROW LEVEL SECURITY ON",
    PqConnection = "ENABLE ROW LEVEL SECURITY"
  )
  invisible(dbExecute(con, glue("ALTER TABLE {table} {enable_chunk}")))
}

#' @export
#' @rdname rls_enable
rls_disable <- function(con, table) {
  is_conn(con)
  enable_chunk <- switch(class(con),
    RedshiftConnection = "ROW LEVEL SECURITY OFF",
    PqConnection = "DISABLE ROW LEVEL SECURITY"
  )
  invisible(dbExecute(con, glue("ALTER TABLE {table} {enable_chunk}")))
}
