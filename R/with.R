#' With database user or role
#'
#' @importFrom withr defer
#' @export
#' @param con (PqConnection) a PostgreSQL DBI connection from
#' [RPostgres::Postgres()]
#' @param user (character) a user or role to switch to to execute
#' the code in `code`
#' @param code a code block
#' @section How it works:
#' Function temporarily assumes role or user as defined in
#' the `user` parameter, executes the code in `code`, then goes
#' back to the user/role outside of scope of the function
#' @section Connections:
#' Currently this function does not close the connection
#' passed to the function, but maybe we should? kinda like
#' `withr::with_db_connection` does?
#' @examplesIf interactive() && has_postgres()
#' library(DBI)
#' library(RPostgres)
#' con <- dbConnect(Postgres())
#'
#' rls_current_user(con)
#'
#' dbCreateTable(con, "cars", mtcars)
#' rows_append(
#'   tbl(con, "cars"),
#'   copy_inline(con, mtcars),
#'   in_place = TRUE
#' )
#'
#' dbExecute(con, "CREATE ROLE analysts")
#'
#' with_db_user(
#'  con = con,
#'  user = "analysts",
#'  code = dbGetQuery(con, "select * from cars")
#' )
with_db_user <- function(con, user, code) {
	original_user <- rls_current_user(con)
	defer(dbExecute(con,
		glue("SET SESSION AUTHORIZATION {original_user}"))
	)
	dbExecute(con,
		glue("SET SESSION AUTHORIZATION {user}")
	)
	force(code)
}
