#' As connection
#' @keywords internal
#' @return an S3 class object
as_con <- function(x) {
  UseMethod("as_con")
}
#' @export
as_con.row_policy <- function(x) {
  return(x$data$src$con)
}
#' @export
as_con.privilege <- function(x) {
  return(x$data$src$con)
}
#' @export
as_con.PqConnection <- function(x) {
  return(x)
}
