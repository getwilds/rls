#' Create a row level security policy
#'
#' @export
#' @param con a DBI database connection object
#' @param policy (list) a policy derived from [rls_construct_policy()]
#' @return NULL
#' @examplesIf interactive() && has_postgres()
#' library(DBI)
#' library(RPostgres)
#' con <- dbConnect(Postgres())
#'
#' dbCreateTable(con, "sometable", mtcars)
#'
#' policy1 <- rls_construct_policy(
#'   name = "hide_confidential",
#'   on = "sometable",
#'   using = "(true)"
#' )
#' policy1
#' rls_create_policy(con, policy1)
#' rls_policies(con)
#'
#' policy2 <- rls_construct_policy(
#'   name = "policy_concerts",
#'   on = "sometable",
#'   for_ = "SELECT",
#'   using = "(true)"
#' )
#' policy2
#' rls_create_policy(con, policy2)
#' rls_policies(con)
#'
#' # cleanup
#' rls_drop_policy(con, policy1)
#' rls_drop_policy(con, policy2)
#' dbDisconnect(con)
rls_create_policy <- function(con, policy) {
  is_conn(con)
  create_statement <- switch(class(con),
    RedshiftConnection = "CREATE RLS",
    PqConnection = "CREATE"
  )
  sql_create_policy <- glue("
    {create_statement} POLICY {policy$name} ON {policy$on}
    {combine_if('FOR', policy$for_)}
    {combine_if('TO', policy$to)}
    {combine_if('USING', policy$using)}
    {combine_if('WITH CHECK', policy$with)}
  ")
  sql_create_policy <- gsub("\n\\s+\n", "\n", sql_create_policy)
  dbExecute(con, sql_create_policy)
}

# {ifelse(!is.null(policy$for_), paste('FOR', policy$for_), '')}
combine_if <- function(statement, item) {
  ifelse(!is.null(item), paste(statement, item), "")
}
