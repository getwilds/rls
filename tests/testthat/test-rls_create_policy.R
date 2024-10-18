test_that("rls_create_policy", {
  with_database_connection({
    DBI::dbWriteTable(con, "mtcars", mtcars, temporary = TRUE)
    on.exit(DBI::dbRemoveTable(con, "mtcars"), add = TRUE)

    policy1 <- rls_construct_policy(
      name = "hide_confidential",
      table = "mtcars",
      using = "(true)"
    )
    x <- rls_create_policy(con, policy1)
    policies <- rls_policies(con)

    expect_type(x, "integer")
    expect_equal(x, 0)
    expect_equal(policies$tablename, "mtcars")
    expect_equal(policies$policyname, "hide_confidential")

    # cleanup
    rls_drop_policy(con, policy1)
  })
})
