#' Setup for running examples throughout this package
#'
#' @name passwd
#' @examplesIf interactive() && has_postgres()
#' con <- dbConnect(Postgres())
#' user <- dbGetQuery(con, "select current_user")$current_user
#' # Create a table
#' ## Create the table (with no data)
#'
#' invisible(dbExecute(con, "
#'   CREATE TABLE passwd (
#'   user_name             text UNIQUE NOT NULL,
#'   pwhash                text,
#'   uid                   int  PRIMARY KEY,
#'   gid                   int  NOT NULL,
#'   real_name             text NOT NULL,
#'   home_phone            text,
#'   home_dir              text NOT NULL,
#'   shell                 text NOT NULL
#' );
#' "))
#'
#' # Some sample data
#' sample_data <- tribble(
#'   ~user_name, ~pwhash, ~uid, ~gid, ~real_name, ~home_phone, ~home_dir, ~shell,
#'   "admin", "xxx", 0, 0, "Admin", "111-222-3333", "/root", "/bin/dash",
#'   "bob", "xxx", 1, 1, "Bob", "123-456-7890", "/home/bob", "/bin/zsh",
#'   "alice", "xxx", 2, 1, "Alice", "098-765-4321", "/home/alice", "/bin/zsh"
#' )
#'
#' # Append rows to the `passwd` table
#' rows_append(
#'   tbl(con, "passwd"),
#'   copy_inline(con, sample_data),
#'   in_place = TRUE
#' )
#'
#' # Check that the data is in the table
#' tbl(con, "passwd")
NULL
