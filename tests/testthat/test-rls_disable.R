test_that("rls_disable", {
  with_database_connection({
    DBI::dbWriteTable(con, "iris", iris, temporary = TRUE)
    on.exit(DBI::dbRemoveTable(con, "iris"), add = TRUE)

    enabled <- rls_enable(con, table = "iris")
    status_before <- rls_check_status(con, "iris")
    disabled <- rls_disable(con, table = "iris")
    status_after <- rls_check_status(con, "iris")

    expect_equal(enabled, 0)
    expect_equal(disabled, 0)
    expect_true(status_before$relrowsecurity)
    expect_false(status_after$relrowsecurity)
  })
})
