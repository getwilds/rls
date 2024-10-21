#' As s3 privilege
#' @name privileges
NULL

#' As s3 privilege
#' @keywords internal
as_privilege <- function(user = NULL, on = NULL, grant = NULL) {
  structure(list(
    user = user,
    on = on,
    grant = grant
  ), class = "privilege")
}

#' @export
print.privilege <- function(x, ...) {
  cat("<privilege>\n")
  for (i in seq_along(x)) {
    cat(glue("  {names(x)[i]}: {x[[i]]}\n", .trim = FALSE))
  }
}

#' User
#'
#' @importFrom rlang as_name enquo
#' @export
#' @rdname privileges
#' @param name (character) a user (role) name
#' @examples
#' user(jane)
user <- function(name) {
  as_privilege(user = as_name(enquo(name)))
}

#' On
#'
#' @importFrom rlang names2 enquos
#' @export
#' @rdname privileges
#' @param .data an s3 object of class `privilege`
#' @param table a table name
#' @param ... column names, 0 or more
#' @examples
#' user(jane) |> on(fruits, apples)
#' user(jane) |> on(fruits, apples, bananas, pears)
on <- function(.data, table, ...) {
  quos <- enquos(...)
  is_named <- (names2(quos) != "")
  # named_quos <- quos[is_named]
  unnamed_quos <- quos[!is_named]
  if (length(unnamed_quos)) {
    unnamed_quos <- unname(vapply(unnamed_quos, as_name, ""))
  }
  .data$on <- list(
    table = as_name(enquo(table)),
    columns = unnamed_quos
  )
  .data
}

#' Grant
#'
#' @export
#' @rdname privileges
#' @param .data an s3 object of class `privilege`
#' @param command one of all, select, update, insert, delete
#' @examples
#' user(jane) |> on(fruits, apples) |> grant(select)
grant <- function(.data, command) {
  .data$grant <- toupper(as_name(enquo(command)))
  .data
}

#' Grant
#'
#' @importFrom glue glue_sql
#' @export
#' @rdname privileges
#' @param priv an s3 object of class `privilege`, required
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
#' #  (apples, strawberries)
#' #  ON fruits
#' #  TO jane
#' sql <- user(jane) |>
#'   on(fruits, apples, strawberries) |>
#'   grant(select) |>
#'   translate_privilege(con)
#' dbExecute(con, sql)
#' # can't pipe into dbExecute for some reason
#' # could in theory include dbExecute in translate_privilege?
translate_privilege <- function(priv, con) {
  stopifnot(inherits(priv, "privilege"))
  sql <- glue_sql("
    GRANT command
    ({`columns`*})
    ON {`priv$on$table`}
    TO {`priv$user`}
    ", priv = priv, columns = priv$on$columns, .con = con
  )
  sub("command", priv$grant, sql)
}
