## Adapted from @smbache Stefan Milton Bache's work in jqr

rls_env <- new.env()

#' Pipe operator
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL

#' Turn on or off auto execution of sql commands
#'
#' @export
#' @param x (logical) turn on auto excecution of SQL commands (`TRUE`),
#' or turn off (`FALSE`; default)
#' @return `NULL`
auto_pipe <- function(x = FALSE) {
  assert_is(x, "logical")
  assert_len(x, 1)
  rls_env$auto_pipe <- x
}

#' Information on Potential Pipeline
#'
#' This function figures out whether it is called from within a pipeline.
#' It does so by examining the parent evironment of the active system frames,
#' and whether any of these are the same as the enclosing environment of
#' `\%>\%`.
#'
#' @return A list with the values `is_piped` (logical) and `env`
#'   (an environment reference). The former is `TRUE` if a pipeline is
#'   identified as `FALSE` otherwise. The latter holds a reference to
#'   the `\%>\%` frame where the pipeline is created and evaluated.
#'
#' @noRd
pipeline_info <- function() {
  parents <- lapply(sys.frames(), parent.env)

  is_magrittr_env <-
    vapply(parents, identical, logical(1), y = environment(`%>%`))

  is_piped <- any(is_magrittr_env)

  list(
    is_piped = is_piped,
    env = if (is_piped) sys.frames()[[max(which(is_magrittr_env))]]
  )
}

#' Toggle Auto Execution On or Off for Pipelines
#'
#' A call to `pipe_autoexec` allows a function to toggle auto execution of
#' `jq` on or off at the end of a pipeline.
#'
#' @param toggle logical: `TRUE` toggles auto execution on, `FALSE`
#'   toggles auto execution off.
#'
#' @details Once auto execution is turned on the `result` identifier inside
#' the pipeline is bound to an "Active Binding". This will not be changed on
#' toggling auto execution off, but rather the function to be executed is
#' changed to `identity`.
#'
#' @noRd
pipe_autoexec <- function(toggle) {
  if (!identical(toggle, TRUE) && !identical(toggle, FALSE)) {
    stop("Argument 'toggle' must be logical.")
  }

  info <- pipeline_info()

  if (isTRUE(info[["is_piped"]])) {
    rls_exit <- function(x) {
      if (inherits(x, c("privilege", "row_policy"))) {
        rls_perform(x, x$data$src$con)
      } else {
        x
      }
    }
    pipeline_on_exit(info$env)
    info$env$.rls_exitfun <- if (toggle) rls_exit else identity
  }

  invisible()
}

#' Setup On-Exit Action for a Pipeline
#'
#' A call to `pipeline_on_exit` will setup the pipeline for auto
#' execution by overriding the return value from an `on.exit`
#' expression pushed in the magrittr frame.
#'
#' @param env A reference to the `\%>\%` environment.
#' @global .rls_exitfun
#' @noRd
pipeline_on_exit <- function(env) {
  # Only activate the first time; after this the binding is already active.
  if (
    exists(".rls_exitfun", envir = env, inherits = FALSE, mode = "function")
  ) {
    return(invisible())
  }
  env$.rls_exitfun <- identity

  # Need to be a bit careful with scoping since `env` is foreign. We
  # inline closures in calls with `do.call()` and `as.call()`. Usage
  # of `do.call()` instead of `eval()` is necessary so that
  # `on.exit()`, `return()`, and `returnValue()` are evaluated in the
  # correct environment upstack and not in the duplicate call frame
  # that `eval()` pushes on the stack.
  exit_clo <- function() {
    out <- do.call(returnValue, list(), envir = env)

    # Will be `NULL` in case of error. This doesn't matter here since
    # we only modify return values that inherit from `"jqr"`.
    if (!is.null(out)) {
      do.call("return", alist(.rls_exitfun(returnValue())), envir = env)
    }
  }

  do.call(on.exit, list(as.call(list(exit_clo)), add = TRUE), envir = env)
}
