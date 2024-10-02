test_that("rls_check_status", {
  with_database_connection({
    DBI::dbWriteTable(con, "attitude", attitude, temporary = TRUE)
    on.exit(DBI::dbRemoveTable(con, "attitude"), add = TRUE)

    rls_enable(con, table = "attitude")
    status_enabled <- rls_check_status(con, "attitude")
    rls_disable(con, table = "attitude")
    status_disabled <- rls_check_status(con, "attitude")

    expect_s3_class(status_enabled, "tbl")
    expect_equal(status_enabled$relname, "attitude")
    expect_true(status_enabled$relrowsecurity)
    expect_s3_class(status_disabled, "tbl")
    expect_equal(status_disabled$relname, "attitude")
    expect_false(status_disabled$relrowsecurity)
  })
})
