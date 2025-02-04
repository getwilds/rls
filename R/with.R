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
#' current1 <- rls_current_user(con)
#' current1
#'
#' dbCreateTable(con, "cars", mtcars)
#' rows_append(
#'   tbl(con, "cars"),
#'   copy_inline(con, mtcars),
#'   in_place = TRUE
#' )
#'
#' dbExecute(con, "CREATE ROLE analysts")
#' dbExecute(con, "GRANT SELECT ON TABLE cars TO analysts")
#'
#' with_db_user(
#'  con = con,
#'  user = "analysts",
#'  code = dbGetQuery(con, "select * from cars")
#' )
#' # current user should be the same as above
#' current2 <- rls_current_user(con)
#' identical(current1, current2)
#' 
#' # cleanup
#' dbRemoveTable(con, "cars")
#' dbExecute(con, "DROP ROLE analysts")
#' dbDisconnect(con)
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
