#' RLS persmissions
#' 
#' Applies to column and row level policies
#' 
#' @export
#' @return list of length 2 with slots for `view`, `edit`
#' @examples
#' rls_permissions()
rls_permissions <- function() {
	list(
		view = "select",
		edit = c("update", "insert", "delete")
	)
}

#' Get the current user 
#' 
#' @export
#' @param con a postgres or redshift connection object
#' @return the current user, scalar
#' @examplesIf has_postgres()
#' library(RPostgres)
#' con <- dbConnect(Postgres())
#' rls_current_user(con)
rls_current_user <- function(con) {
	dbGetQuery(con, "select current_user")$current_user
}

#' tbl variant for rls
#'
#' @importFrom dplyr tbl filter
#' @importFrom dbplyr sql
#' @export
#' @param con a postgres or redshift connection object
#' @param from (character) a table name
#' @param ... args passed on to [dplyr::tbl()]
#' @autoglobal
#' @return a `tbl`
#' @examplesIf has_postgres()
#' library(RPostgres)
#' con <- dbConnect(Postgres())
#' dbExecute(con, "SET SESSION AUTHORIZATION aliceuser")
#' rls_tbl(con, "passwd")
#' dbExecute(con, "SET SESSION AUTHORIZATION schambe3")
#' rls_tbl(con, "passwd")
rls_tbl <- function(con, from, ...) {
	privs <- rls_column_privileges(con, from, rls_current_user(con))
	sql_custom <- if (NROW(privs) == 0) {
		sql(glue("SELECT * FROM {from}"))
	} else {
		privs <- dplyr::filter(privs, privilege_type == "SELECT")
		sql(sprintf("SELECT %s FROM %s", paste(privs$column_name, collapse = ", "), from))
	}
	tbl(con, sql_custom, ...)
}

#' Column level privileges
#' 
#' @export
#' @param con a postgres or redshift connection object
#' @param table (character) a table name
#' @param user_role (character) a user or role name
#' @param schema (character) a schema
#' @return a tbl with `column_name` and `privilege_type`
#' @examplesIf has_postgres()
#' library(RPostgres)
#' con <- dbConnect(Postgres())
#' dbExecute(con, "GRANT SELECT
#'    (user_name, uid, gid, real_name, home_phone, home_dir, shell)
#'    ON passwd TO aliceuser"
#'  )
#' rls_column_privileges(con, "passwd", "aliceuser")
rls_column_privileges <- function(con, table, user_role, schema = "public") {
	as_tibble(dbGetQuery(con, glue("
		SELECT
			c.column_name,
			p.privilege_type
		FROM
			information_schema.role_column_grants p
		JOIN
			information_schema.columns c
		ON
			p.table_name = c.table_name AND p.column_name = c.column_name
		WHERE
			p.grantee = '{user_role}' AND
			c.table_name = '{table}' AND
			c.table_schema = '{schema}';
	")))
}

#' Table level privileges
#' 
#' @export
#' @inheritParams rls_column_privileges
#' @return a tbl with whether user or role has privileges on a table for
#' each of the main commands: select, insert, update, delete
#' @examplesIf has_postgres()
#' library(RPostgres)
#' con <- dbConnect(Postgres())
#' rls_table_privileges(con, "passwd")
#' rls_table_privileges(con, "orange")
rls_table_privileges <- function(con, table, schema = "public") {
	as_tibble(dbGetQuery(con, glue("
		SELECT a.schemaname, a.tablename, b.usename,
		  HAS_TABLE_PRIVILEGE(usename, quote_ident(schemaname) || '.' || quote_ident(tablename), 'select') as select,
		  HAS_TABLE_PRIVILEGE(usename, quote_ident(schemaname) || '.' || quote_ident(tablename), 'insert') as insert,
		  HAS_TABLE_PRIVILEGE(usename, quote_ident(schemaname) || '.' || quote_ident(tablename), 'update') as update,
		  HAS_TABLE_PRIVILEGE(usename, quote_ident(schemaname) || '.' || quote_ident(tablename), 'delete') as delete
			FROM pg_tables a, pg_user b
			WHERE a.schemaname = '{schema}' AND a.tablename='{table}'
	")))
}

#' List roles
#' 
#' @import dbplyr
#' @export
#' @param con a postgres or redshift connection object
#' @autoglobal
#' @global %like%
#' @return a `tbl`
#' @examplesIf has_postgres()
#' library(RPostgres)
#' con <- dbConnect(Postgres())
#' rls_list_roles(con)
rls_list_roles <- function(con) {
	tbl(con, "pg_roles") %>%
		filter(
			rolname != "postgres",
			!rolname %like% "pg_%"
		)
}

#' Column policies
#' 
#' @export
#' @param .data lazy_frame or data.frame or tbl, etc.
#' @param role (character) the role name
#' @param permissions (character) one of "view" or "edit", see details
#' @section Permissions:
#' - view: select
#' - edit: update, insert, delete
#' @examplesIf has_postgres() && rlang::is_installed("dbplyr")
#' library(RPostgres)
#' library(dbplyr)
#' con <- dbConnect(Postgres())
#' rls_tbl(con, "passwd") %>% rls_col_policy(role = "public", permissions = "view")
#' df <- rls_tbl(con, "passwd") %>% dplyr::collect()
#' dbplyr::lazy_frame(df) %>% 
#'   rls_col_policy(role = "public", permissions = "view") %>% 
#'   attr(., "policies_columns")
rls_col_policy <- function(.data, role = NULL, permissions = NULL) {
	attr(.data, "policies_columns") <- list(role = role, permissions = permissions)
	.data
}
