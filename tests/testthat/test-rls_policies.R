test_that("rls_policies", {
  with_database_connection({
    # first, drop any existing tables
    tables <- DBI::dbListTables(con)
    if (length(tables)) {
      invisible(lapply(tables, \(x) DBI::dbRemoveTable(con, x)))
    }

    DBI::dbWriteTable(con, "attitude", attitude,
      overwrite = TRUE, temporary = TRUE)
    on.exit(DBI::dbRemoveTable(con, "attitude"), add = TRUE)

    my_policy <- rls_tbl(con, "attitude") %>%
      row_policy("all_view") %>%
      commands(select) %>%
      rows_existing(TRUE)

    rls_run(my_policy)

    policies <- rls_policies(con)

    expect_s3_class(policies, "tbl")
    expect_equal(NROW(policies), 1)
    expect_equal(policies$tablename, "attitude")
    expect_equal(unique(policies$permissive), "PERMISSIVE")

    # cleanup
    rls_drop_policy(con, my_policy)
  })
})
