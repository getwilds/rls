test_that("rls_drop_policy", {
  with_database_connection({
    rls_drop_policies(con)
    DBI::dbWriteTable(con, "usarrests", USArrests,
      overwrite = TRUE, temporary = TRUE)
    on.exit(DBI::dbRemoveTable(con, "usarrests"), add = TRUE)

    the_policy <- rls_tbl(con, "usarrests") %>%
      row_policy("hide_confidential") %>%
      rows_existing(TRUE)

    rls_perform(the_policy)

    policies_before <- rls_policies(con)
    out <- rls_drop_policy(con, the_policy)
    policies_after <- rls_policies(con)

    expect_type(out, "integer")
    expect_equal(out, 0)
    expect_equal(policies_before$tablename, "usarrests")
    expect_equal(policies_before$policyname, "hide_confidential")
    expect_equal(policies_after$tablename, character(0))
    expect_equal(policies_after$policyname, character(0))
  })
})
