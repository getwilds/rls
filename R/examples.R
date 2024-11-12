#' Setup for running examples throughout this package
#'
#' @aliases passwd
#' @export
#' @importFrom tibble tribble
#' @importFrom dbplyr copy_inline
#' @importFrom dplyr rows_append
#' @param con a postgres or redshift connection object
#' @param which (character) the table to create. only option right
#' now is "passwd"
#' @examplesIf interactive() && has_postgres()
#' library(RPostgres)
#' library(dplyr)
#' library(dbplyr)
#' library(tibble)
#'
#' con <- dbConnect(Postgres())
#'
#' # Create a table
#' ## Create the table (with no data)
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
#' ## The data
#' sample_data <- tribble(
#'   ~user_name, ~pwhash, ~uid, ~gid, ~real_name, ~home_phone, ~home_dir, ~shell,
#'   "admin", "xxx", 0, 0, "Admin", "111-222-3333", "/root", "/bin/dash",
#'   "bob", "xxx", 1, 1, "Bob", "123-456-7890", "/home/bob", "/bin/zsh",
#'   "alice", "xxx", 2, 1, "Alice", "098-765-4321", "/home/alice", "/bin/zsh"
#' )
#'
#' ## Append rows to the `passwd` table
#' rows_append(
#'   tbl(con, "passwd"),
#'   copy_inline(con, sample_data),
#'   in_place = TRUE
#' )
#'
#' ## Check that the data is in the table
#' tbl(con, "passwd")
setup_example_table <- function(con, which = "passwd") {
	options <- c("passwd")
	if (!which %in% options) {
		rls_abort(
			format_error("{.arg {which}} must be one of {clz_col(options)}")
		)
	}
	dbExecute(con, eg_schemas[[which]])
	rows_append(
	  tbl(con, which),
	  copy_inline(con, eg_data[[which]]),
	  in_place = TRUE
	)
	tbl(con, which)
}

eg_schemas <- list(
	passwd = "
	  CREATE TABLE passwd (
	  user_name             text UNIQUE NOT NULL,
	  pwhash                text,
	  uid                   int  PRIMARY KEY,
	  gid                   int  NOT NULL,
	  real_name             text NOT NULL,
	  home_phone            text,
	  home_dir              text NOT NULL,
	  shell                 text NOT NULL
	);
	"
)

eg_data <- list(
	passwd = tribble(
	  ~user_name, ~pwhash, ~uid, ~gid, ~real_name, ~home_phone, ~home_dir,~shell,
	  "admin", "xxx", 0, 0, "Admin", "111-222-3333", "/root", "/bin/dash",
	  "bob", "xxx", 1, 1, "Bob", "123-456-7890", "/home/bob", "/bin/zsh",
	  "alice", "xxx", 2, 1, "Alice", "098-765-4321", "/home/alice", "/bin/zsh"
	)
)
