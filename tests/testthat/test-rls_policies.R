test_that("rls_policies", {
  with_database_connection({
    DBI::dbWriteTable(con, "attitude", attitude, temporary = TRUE)
    on.exit(DBI::dbRemoveTable(con, "attitude"), add = TRUE)

    my_policy <- rls_construct_policy(
      name = "all_view",
      table = "attitude",
      command = "SELECT",
      using = "(true)"
    )
    rls_create_policy(con, my_policy)
    policies <- rls_policies(con)

    expect_s3_class(policies, "tbl")
    expect_equal(NROW(policies), 1)
    expect_equal(policies$tablename, "attitude")
    expect_equal(unique(policies$permissive), "PERMISSIVE")

    # cleanup
    rls_drop_policy(con, my_policy)
  })
})
