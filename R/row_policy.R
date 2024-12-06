#' Row policy
#' 
#' @export
#' @inheritParams grant
#' @param name (character) scalar name for the policy. required
#' @examplesIf has_postgres()
#' library(DBI)
#' library(RPostgres)
#' con <- dbConnect(Postgres())
#' if (!dbExistsTable(con, "passwd")) {
#'    setup_example_table(con, "passwd")
#' }
#' rls_tbl(con, "passwd") %>%
#'   row_policy("my_policy") %>%
#'   rls_run()
#' rls_policies(con)
#' 
#' # cleanup
#' rls_drop_policies(con)
#' dbDisconnect(con)
row_policy <- function(.data, name) {
  pipe_autoexec(toggle = rls_env$auto_pipe)
  assert_is(name, "character")
  assert_scalar(name)
  .data <- as_row_policy(.data)
  .data$name <- name
  .data
}

#' Commands
#' 
#' @export
#' @inheritParams grant
#' @examplesIf has_postgres()
#' library(DBI)
#' library(RPostgres)
#' con <- dbConnect(Postgres())
#' if (!dbExistsTable(con, "passwd")) {
#'    setup_example_table(con, "passwd")
#' }
#' rls_tbl(con, "passwd") %>%
#'   row_policy("their_policy") %>%
#'   commands(update)
#' 
#' # cleanup
#' dbDisconnect(con)
commands <- function(.data, ...) {
  pipe_autoexec(toggle = rls_env$auto_pipe)
  .data <- as_row_policy(.data)
  .data$commands <- dot_names(...)
  .data
}

#' Create rule for existing rows
#' 
#' @export
#' @inheritParams grant
#' @param using an expression to use to check against existing rows
#' @param sql (character) sql syntax to use for existing rows
#' @details Use either `using` or `sql`, not both
#' @examplesIf has_postgres()
#' library(DBI)
#' library(RPostgres)
#' con <- dbConnect(Postgres())
#' if (!dbExistsTable(con, "passwd")) {
#'    setup_example_table(con, "passwd")
#' }
#' rls_tbl(con, "passwd") %>%
#'   row_policy("a_good_policy") %>%
#'   commands(update) %>%
#'   rows_existing(sql = 'current_user = "user_name"')
#' 
#' # cleanup
#' dbDisconnect(con)
rows_existing <- function(.data, using = NULL, sql = NULL) {
  pipe_autoexec(toggle = rls_env$auto_pipe)
  using_quo <- enquo(using)
  stopifnot("Can not using and sql parameters together" =
    xor(!rlang::quo_is_null(using_quo), !is_empty(sql)))
  .data <- as_row_policy(.data)
  if (rlang::is_null(sql)) {
    .data$existing_rows <- translate_sql(!!using_quo, con = as_con(.data))
  } else {
    .data$existing_rows <- sql
  }
  .data
}

#' Create rule for new rows
#' 
#' @export
#' @importFrom dbplyr translate_sql
#' @inheritParams grant
#' @param check an expression to use to check against addition of
#' new rows or editing of existing rows
#' @param sql (character) sql syntax to use for new rows
#' @details Use either `check` or `sql`, not both
#' @examplesIf has_postgres()
#' library(DBI)
#' library(RPostgres)
#' con <- dbConnect(Postgres())
#' if (!dbExistsTable(con, "passwd")) {
#'    setup_example_table(con, "passwd")
#' }
#'
#' rls_tbl(con, "passwd") %>%
#'   row_policy("a_policy") %>%
#'   commands(update) %>%
#'   rows_existing(TRUE) %>%
#'   rows_new(TRUE) %>%
#'   to(jane)
#'
#' rls_tbl(con, "passwd") %>%
#'   row_policy("that_policy") %>%
#'   commands(update) %>%
#'   rows_existing(sql = 'current_user = "user_name"') %>%
#'   rows_new(home_phone == "098-765-4321") %>%
#'   to(jane)
#' 
#' # cleanup
#' dbDisconnect(con)
rows_new <- function(.data, check = NULL, sql = NULL) {
  pipe_autoexec(toggle = rls_env$auto_pipe)
  check_quo <- enquo(check)
  stopifnot("Can not check and sql parameters together" =
    xor(!rlang::quo_is_null(check_quo), !is_empty(sql)))
  .data <- as_row_policy(.data)
  if (rlang::is_null(sql)) {
    .data$new_rows <- translate_sql(!!check_quo, con = as_con(.data))
  } else {
    .data$new_rows <- sql
  }
  .data
}

#' @note param `fun` takes a function, by default uses a function
#' that simply returns whatever is passed in to it
#' @noRd
combine_if <- function(statement, item, fun = \(x) x) {
  ifelse(!rlang::is_null(item), paste(statement, fun(item)), "")
}

express <- function(x) {
  glue("({ifelse(x == 'TRUE', tolower(x), x)})")
}

#' Translate row policy
#'
#' @export
#' @keywords internal
#' @param policy an S3 object of class `row_policy`, required
#' @param con DBI connection object, required
#' @references <https://www.postgresql.org/docs/current/sql-createpolicy.html>
#' @examplesIf has_postgres()
#' library(DBI)
#' library(RPostgres)
#' con <- dbConnect(Postgres())
#' setup_example_table(con)
#' 
#' # create role
#' dbExecute(con, "CREATE ROLE jane")
#'
#' if (rls_policy_exists(con, "blue_policy")) {
#'   rls_drop_policy(con, name = "blue_policy", table = "passwd")
#' }
#'
#' policy <- rls_tbl(con, "passwd") %>%
#'   row_policy(name = "blue_policy") %>%
#'   commands(update) %>%
#'   rows_existing(TRUE) %>%
#'   rows_new(TRUE) %>%
#'   to(jane)
#' policy
#' sql <- translate_row_policy(policy, con)
#' sql
#' dbExecute(con, sql)
#' 
#' # cleanup
#' rls_drop_policies(con)
#' dbExecute(con, "DROP ROLE jane")
#' dbDisconnect(con)
translate_row_policy <- function(policy, con) {
  is_conn(con)
  create_statement <- switch(class(con),
    RedshiftConnection = "CREATE RLS",
    PqConnection = "CREATE"
  )
  sql_create_policy <- glue("
    {create_statement} POLICY {policy$name} ON {attr(policy$data, 'table')}
    {combine_if('FOR', policy$commands)}
    {combine_if('TO', policy$user)}
    {combine_if('USING', policy$existing_rows, express)}
    {combine_if('WITH CHECK', policy$new_rows, express)}
  ")
  sql(gsub("\n\\s+\n", "\n", sql_create_policy))
}
