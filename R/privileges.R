#' Grant
#'
#' @export
#' @param .data an s3 object of class `privilege`
#' @param ... one of all, select, update, insert, delete
#' @param cols (character) vector of column names
#' @inherit as_priv return
#' @examplesIf has_postgres()
#' library(DBI)
#' library(RPostgres)
#' con <- dbConnect(Postgres())
#' if (!dbExistsTable(con, "passwd")) {
#'    setup_example_table(con, "passwd")
#' }
#' 
#' if (!rls_role_exists(con, "jane")) {
#'   dbExecute(con, "CREATE ROLE jane")
#' }
#'
#' # NOTE: privileges construction doesn't make a change in the database
#' # unless you pass it to `rls_perform` or set `auto_pipe(TRUE)`
#' rls_tbl(con, "passwd") %>%
#'   grant(update) %>%
#'   to(jane)
#'
#' rls_tbl(con, "passwd") %>%
#'   grant(update, delete) %>%
#'   to(jane)
#'
#' rls_tbl(con, "passwd") %>%
#'   grant(update, select, cols = c("real_name", "home_phone")) %>%
#'   to(jane)
#' 
#' # Execute a privilege and inspect the change to jane's details in the DB
#' rls_tbl(con, "passwd") %>%
#'   grant(update, delete) %>%
#'   to(jane) %>% 
#'   rls_perform()
#' rls_column_privileges(con, "passwd", "jane")
#' 
#' # cleanup
#' dbExecute(con, "DROP OWNED BY jane")
#' dbExecute(con, "DROP ROLE jane")
#' dbDisconnect(con)
grant <- function(.data, ..., cols = NULL) {
  pipe_autoexec(toggle = rls_env$auto_pipe)
  .data <- as_priv(.data)
  .data$type <- "grant"
  .data$privilege <- append(
    .data$privilege,
    list(
      rls_grant(toupper(dot_names(...)), cols %||% "")
    )
  )
  .data
}

#' Revoke
#'
#' @export
#' @inheritParams grant
#' @inherit as_priv return
#' @examplesIf has_postgres()
#' library(DBI)
#' library(RPostgres)
#' con <- dbConnect(Postgres())
#' if (!dbExistsTable(con, "passwd")) {
#'    setup_example_table(con, "passwd")
#' }
#' 
#' if (!rls_role_exists(con, "jill")) {
#'   dbExecute(con, "CREATE ROLE jill")
#' }
#' 
#' # NOTE: privileges construction doesn't make a change in the database
#' # unless you pass it to `rls_perform` or set `auto_pipe(TRUE)`
#' 
#' # Grant first
#' rls_tbl(con, "passwd") %>%
#'   grant(update) %>%
#'   from(jill)
#'
#' # Then revoke
#' rls_tbl(con, "passwd") %>%
#'   revoke(update) %>%
#'   from(jill)
#'
#' # Revoke on certain columns
#' rls_tbl(con, "passwd") %>%
#'   revoke(update, cols = c("real_name", "home_phone")) %>%
#'   from(jill)
#' 
#' # cleanup
#' dbExecute(con, "DROP ROLE jill")
#' dbDisconnect(con)
revoke <- function(.data, ..., cols = NULL) {
  pipe_autoexec(toggle = rls_env$auto_pipe)
  .data <- as_priv(.data)
  .data$type <- "revoke"
  .data$privilege <- append(
    .data$privilege,
    list(
      rls_revoke(toupper(dot_names(...)), cols %||% "")
    )
  )
  .data
}

#' Grant or revoke TO or FROM a role or user
#'
#' @export
#' @param .data a `privilege` object
#' @param ... (character) one or more user (or role) names
#' @details `to()` and `from()` are the same exact code underneath; the former
#' exists as it sounds better with `grant()` while the latter makes more sense
#' with `revoke()`; but, you can use them interchangably
#' @inherit as_priv return
#' @examplesIf interactive() && has_postgres()
#' library(DBI)
#' library(RPostgres)
#' con <- dbConnect(Postgres())
#' if (!dbExistsTable(con, "passwd")) {
#'    setup_example_table(con, "passwd")
#' }
#'
#' rls_tbl(con, "passwd") %>% grant(select) %>% to(jane)
#' rls_tbl(con, "passwd") %>% revoke(select) %>% from(jane)
#' rls_tbl(con, "passwd") %>% grant(select) %>% to(jane, bob, alice)
#' 
#' # Errors: doesn't make sense to pass rls_tbl output directly to to/from
#' # rls_tbl(con, "passwd") %>% from(jane)
#' # #> ! must pass privilege or row_policy to to/from
#' 
#' # cleanup
#' dbDisconnect(con)
to <- function(.data, ...) {
  pipe_autoexec(toggle = rls_env$auto_pipe)
  assert_is(.data, c("privilege", "row_policy", "tbl_sql"))
  .data <- switch_multiclass(class(.data),
    privilege = as_priv(.data),
    row_policy = as_row_policy(.data),
    tbl_sql = rls_abort("must pass privilege or row_policy to to/from")
  )
  .data$user <- dot_names(...)
  .data
}

#' @export
#' @rdname to
from <- to

#' Translate privilege
#'
#' @export
#' @keywords internal
#' @param priv an S3 object of class `privilege`, required
#' @param con DBI connection object, required
#' @return an object of S3 class [dplyr::sql()] (it's character under the hood)
#' @examplesIf interactive() && has_postgres()
#' library(tibble)
#' library(RPostgres)
#' library(DBI)
#' con <- dbConnect(Postgres())
#'
#' dat <- tibble(
#'   apples = c("pink lady", "cortland", "mcintosh"),
#'   strawberries = c("alice", "albion", "alaska pioneer")
#' )
#' dbWriteTable(con, "fruits", dat)
#' dbExecute(con, "CREATE ROLE jane")
#' auto_pipe(FALSE)
#'
#' # GRANT SELECT
#' #  ON fruits
#' #  TO jane
#' priv <-
#'   rls_tbl(con, "fruits") %>%
#'   grant(select) %>%
#'   to(jane)
#' priv
#' translate_privilege(priv, con)
#'
#' # REVOKE SELECT
#' #  ON fruits
#' #  FROM jane
#' priv <-
#'   rls_tbl(con, "fruits") %>%
#'   revoke(select) %>%
#'   from(jane)
#' priv
#' translate_privilege(priv, con)
#'
#' # GRANT SELECT
#' #  (apples, strawberries)
#' #  ON fruits
#' #  TO jane
#' priv <-
#'   rls_tbl(con, "fruits") %>%
#'   grant(select, cols = c("apples", "strawberries")) %>%
#'   to(jane)
#' priv
#' sql <- translate_privilege(priv, con)
#' sql
#' dbExecute(con, sql)
#' rls_column_privileges(con, "fruits", "jane")
#' 
#' # cleanup
#' dbRemoveTable(con, "fruits")
#' dbExecute(con, "DROP ROLE jane")
#' dbDisconnect(con)
translate_privilege <- function(priv, con) {
  assert_is(priv, "privilege")
  is_conn(con)

  template <- priv_templates[[priv$type]]

  table_cols <- collapse(lapply(priv$privilege, \(w) {
    cols <- collapse(w$cols)
    sprintf(
      "%s %s",
      collapse(w$commands),
      ifelse(is_really_empty(cols), "", glue("({cols})"))
    )
  }))

  query <- sprintf(
    template,
    table_cols,
    attr(priv$data, "table"),
    priv$user
  )
  query <- trimws_inside(trimws(query, which = "both"))
  sql(query)
}

priv_templates <- list(
  grant = "GRANT %s ON %s TO %s",
  revoke = "REVOKE %s ON %s FROM %s"
)

rls_grant <- function(commands, cols) {
  x <- list(commands = commands, cols = cols)
  structure(x, class = "rls_grant")
}

rls_revoke <- function(commands, cols) {
  x <- list(commands = commands, cols = cols)
  structure(x, class = "rls_revoke")
}
