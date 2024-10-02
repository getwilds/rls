test_that("rls_enable", {
  with_database_connection({
    DBI::dbWriteTable(con, "beaver2", beaver2, temporary = TRUE)
    on.exit(DBI::dbRemoveTable(con, "beaver2"), add = TRUE)

    enabled <- rls_enable(con, table = "beaver2")
    status <- rls_check_status(con, "beaver2")

    expect_equal(enabled, 0)
    expect_true(status$relrowsecurity)
  })
})
