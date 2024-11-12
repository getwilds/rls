#' Grant
#'
#' @export
#' @param .data an s3 object of class `privilege`
#' @param ... one of all, select, update, insert, delete
#' @param cols (character) vector of column names
#' @examplesIf interactive() && has_postgres()
#' library(RPostgres)
#' con <- dbConnect(Postgres())
#'
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
#' @examplesIf interactive() && has_postgres()
#' library(RPostgres)
#' con <- dbConnect(Postgres())
#'
#' rls_tbl(con, "passwd") %>%
#'   revoke(update) %>%
#'   from(jane)
#'
#' rls_tbl(con, "passwd") %>%
#'   revoke(update, cols = c("real_name", "home_phone")) %>%
#'   from(jane)
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

#' To a role or user
#'
#' @export
#' @param .data a `privilege` object
#' @param ... (character) one or more user (or role) names
#' @examplesIf interactive() && has_postgres()
#' library(RPostgres)
#' con <- dbConnect(Postgres())
#' rls_tbl(con, "passwd") %>% to(jane)
#' rls_tbl(con, "passwd") %>% to(jane, bob, alice)
to <- function(.data, ...) {
  pipe_autoexec(toggle = rls_env$auto_pipe)
  .data <- switch(class(.data),
    privilege = as_priv(.data),
    row_policy = as_row_policy(.data)
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
#' @param priv an object of class `privilege`, required
#' @param con DBI connection object, required
#' @examplesIf interactive() && rlang::is_installed("dbplyr")
#' library(tibble)
#' library(RPostgres)
#' library(DBI)
#' con <- dbConnect(Postgres())
#'
#' dat <- tibble(
#'   apples = c("pink lady", "cortland", "mcintosh"),
#'   strawberries = c("alice", "albion", "alaska pioneer")
#' )
#' DBI::dbWriteTable(con, "fruits", dat)
#' dbExecute(con, "CREATE ROLE jane")
#'
#' # GRANT SELECT
#' #  ON fruits
#' #  TO jane
#' priv <-
#'   rls_tbl(con, "fruits") %>%
#'   grant(select) %>%
#'   to(jane)
#' translate_privilege(priv, con)
#'
#' # REVOKE SELECT
#' #  ON fruits
#' #  FROM jane
#' priv <-
#'   rls_tbl(con, "fruits") %>%
#'   revoke(select) %>%
#'   from(jane)
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
#' sql <- translate_privilege(priv, con)
#' dbExecute(con, sql)
translate_privilege <- function(priv, con) {
  assert_is(priv, "privilege")
  # stopifnot("Can not use grant and revoke" =
  #   xor(!is_empty(priv$grant), !is_empty(priv$revoke)))

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

#' Run a query
#'
#' @export
#' @param query an s3 object of class `privilege` or `row_policy, required
#' @param con DBI connection object, required
rls_run <- function(con, query) {
  is_conn(con)
  assert_is(query, c("privilege", "row_policy"))
  sql <- switch(class(query),
    privilege = translate_privilege(query, con),
    row_policy = translate_row_policy(query, con)
  )
  dbExecute(con, sql)
}

rls_grant <- function(commands, cols) {
  x <- list(commands = commands, cols = cols)
  structure(x, class = "rls_grant")
}

rls_revoke <- function(commands, cols) {
  x <- list(commands = commands, cols = cols)
  structure(x, class = "rls_revoke")
}
