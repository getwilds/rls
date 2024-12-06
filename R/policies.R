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
#' library(DBI)
#' library(RPostgres)
#' con <- dbConnect(Postgres())
#' rls_current_user(con)
rls_current_user <- function(con) {
  dbGetQuery(con, "select current_user")$current_user
}

#' Column level privileges
#'
#' @export
#' @param con a postgres or redshift connection object
#' @param table (character) a table name
#' @param user_role (character) a user or role name. default: `NULL`.
#' if `NULL`, all users/roles returned
#' @param schema (character) a schema
#' @return a tbl with `column_name` and `privilege_type`
#' @examplesIf has_postgres()
#' library(DBI)
#' library(RPostgres)
#' con <- dbConnect(Postgres())
#' setup_example_table(con)
#' dbExecute(con, "CREATE ROLE sammy")
#' columns <- c("user_name", "uid", "gid", "real_name", 
#'   "home_phone", "home_dir", "shell")
#' rls_tbl(con, "passwd") %>%
#'   grant(select, cols = columns) %>%
#'   to(sammy) %>%
#'   rls_run()
#' # Just user sammy
#' rls_column_privileges(con, "passwd", "sammy")
#' # All users
#' rls_column_privileges(con, "passwd")
#' 
#' # cleanup
#' dbRemoveTable(con, "passwd")
#' dbExecute(con, "DROP ROLE sammy")
#' dbDisconnect(con)
rls_column_privileges <- function(con, table, user_role = NULL,
                                  schema = "public") {
  userrole <- ""
  if (!rlang::is_null(user_role)) {
    userrole <- glue("p.grantee = '{user_role}' AND")
  }
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
			{userrole}
			c.table_name = '{table}' AND
			c.table_schema = '{schema}';
	")))
}

#' Table level privileges
#'
#' @export
#' @importFrom dplyr bind_rows
#' @inheritParams rls_column_privileges
#' @return a tbl with whether user or role has privileges on a table for
#' each of the main commands: select, insert, update, delete
#' @examplesIf has_postgres()
#' library(DBI)
#' library(RPostgres)
#' library(glue)
#' con <- dbConnect(Postgres())
#' user <- rls_current_user(con)
#' 
#' dbExecute(con, "CREATE ROLE stevie")
#' dbExecute(con, "GRANT CREATE ON SCHEMA PUBLIC TO stevie")
#' dbExecute(con, "SET SESSION AUTHORIZATION stevie")
#' setup_example_table(con)
#' rls_table_privileges(con, "passwd")
#' 
#' # cleanup
#' dbRemoveTable(con, "passwd")
#' dbExecute(con, glue("SET SESSION AUTHORIZATION {user}"))
#' dbExecute(con, "REVOKE CREATE ON SCHEMA PUBLIC FROM stevie")
#' dbExecute(con, "DROP ROLE stevie")
#' dbDisconnect(con)
rls_table_privileges <- function(con, table, schema = "public") {
  bind_rows(
    users = table_privileges_query(con, table, schema, "usename", "pg_user"),
    roles = table_privileges_query(con, table, schema, "rolname", "pg_roles",
      "AND b.rolname != 'postgres' 
      AND b.rolname NOT LIKE 'pg_%'"
    ),
    .id = "type"
  )
}

table_privileges_query <- function(con, table, schema, user_role, urtable,
  extra_where = "") {
  
  as_tibble(dbGetQuery(con, glue("
		SELECT a.schemaname, a.tablename, b.{user_role} AS name,
		  HAS_TABLE_PRIVILEGE({user_role},
        quote_ident(schemaname) || '.' || quote_ident(tablename), 'select') as select,
		  HAS_TABLE_PRIVILEGE({user_role}, 
        quote_ident(schemaname) || '.' || quote_ident(tablename), 'insert') as insert,
		  HAS_TABLE_PRIVILEGE({user_role},
        quote_ident(schemaname) || '.' || quote_ident(tablename), 'update') as update,
		  HAS_TABLE_PRIVILEGE({user_role},
        quote_ident(schemaname) || '.' || quote_ident(tablename), 'delete') as delete
			FROM pg_tables a, {urtable} b
			WHERE a.schemaname = '{schema}'
      AND a.tablename='{table}'
      {extra_where}
	")))
}

#' Overview of privileges and policies
#'
#' @export
#' @inheritParams rls_column_privileges
#' @details *Privileges* are broken down into two categories:
#' - **Table**: Some privileges can only be thought about at the table level,
#' e.g., truncate can only be applied to an entire table, not a column
#' - **Column**: Column level priveleges specify access to
#'
#' Then there's **Row** level *policies*, which as the name says apply to
#' specific rows only
#'
#' Both table and column level privileges use the SQL command `GRANT`,
#' while row level policies use a separate command `CREATE POLICY`.
#' Row level policies have specific names to them - whereas table and
#' column level privileges do not have names.
#' @section Running examples:
#' First run the code in [passwd], then run the below code
#' @examplesIf has_postgres()
#' library(DBI)
#' library(RPostgres)
#' con <- dbConnect(Postgres())
#' setup_example_table(con)
#' dbExecute(con, "CREATE ROLE jane")
#' 
#' rls_privileges(con, "passwd")
#' 
#' rls_tbl(con, "passwd") %>%
#'   row_policy(name = "stuff") %>%
#'   rows_existing(TRUE) %>%
#'   to(jane) %>% 
#'   rls_run()
#' 
#' rls_privileges(con, "passwd", "jane")
#' 
#' # cleanup
#' dbRemoveTable(con, "passwd")
#' dbExecute(con, "DROP ROLE jane")
#' dbDisconnect(con)
rls_privileges <- function(con, table, user_role = NULL, schema = "public") {
  list(
    table = rls_table_privileges(con, table, schema),
    column = rls_column_privileges(con, table, user_role, schema),
    row = rls_policies(con)
  )
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
#' library(DBI)
#' library(RPostgres)
#' con <- dbConnect(Postgres())
#' rls_list_roles(con)
#' 
#' # cleanup
#' dbDisconnect(con)
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
#' @examplesIf has_postgres()
#' library(DBI)
#' library(RPostgres)
#' library(dbplyr)
#' con <- dbConnect(Postgres())
#' setup_example_table(con)
#' rls_tbl(con, "passwd") %>% rls_col_policy(role = "public", permissions = "view")
#' df <- rls_tbl(con, "passwd") %>% dplyr::collect()
#' dbplyr::lazy_frame(df) %>%
#'   rls_col_policy(role = "public", permissions = "view") %>%
#'   attr(., "policies_columns")
#' 
#' # cleanup
#' dbRemoveTable(con, "passwd")
#' dbDisconnect(con)
rls_col_policy <- function(.data, role = NULL, permissions = NULL) {
  attr(.data, "policies_columns") <- list(role = role, permissions = permissions)
  .data
}
