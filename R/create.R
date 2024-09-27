#' Create an RLS policy
#'
#' @export
#' @importFrom glue glue
#' @importFrom DBI dbSendQuery
#' @param con a DBI database connection object
#' @param policy (list) a policy derived from [rls_construct_policy()]
#' @return NULL
#' @examplesIf interactive()
#' policy1 <- rls_construct_policy(
#'   name = "hide_confidential",
#'   with = "confidential BOOLEAN",
#'   using = "confidential = false"
#' )
#' policy1
#' rls_create_policy(con, policy)
#' rls_policies(con)
#'
#' policy2 <- rls_construct_policy(
#'   name = "policy_concerts",
#'   with = "catgroup VARCHAR(10)",
#'   using = "catgroup = 'Concerts'"
#' )
#' policy2
#' rls_create_policy(con, policy2)
#' rls_policies(con)
rls_create_policy <- function(con, policy) {
  sql_create_policy <- glue("
    CREATE RLS POLICY {policy$name}
    WITH ({policy$with})
    USING ({policy$using})
  ")
  dbSendQuery(con, sql_create_policy)
}
