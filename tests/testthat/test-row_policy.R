test_that("row_policy - errors as expected", {
  with_database_connection({
    # con <- DBI::dbConnect(RPostgres::Postgres())
    DBI::dbWriteTable(con, "mtcars", mtcars,
      overwrite = TRUE, temporary = TRUE)
    on.exit(DBI::dbRemoveTable(con, "mtcars"), add = TRUE)

    expect_error(row_policy(), "\"name\" is missing")
    expect_error(row_policy(5), "\"name\" is missing")
    expect_error(row_policy(rls_tbl(con, "mtcars")), "\"name\" is missing")
    expect_error(row_policy(rls_tbl(con, "mtcars"), name = 5), "must be class")
    expect_error(row_policy(rls_tbl(con, "mtcars"), name = letters),
      "must be scalar")
  })
})

test_that("row_policy - policy structure is as expected", {
  with_database_connection({
    DBI::dbWriteTable(con, "mtcars", mtcars,
      overwrite = TRUE, temporary = TRUE)
    on.exit(DBI::dbRemoveTable(con, "mtcars"), add = TRUE)

    policy1 <- rls_tbl(con, "mtcars") %>%
      row_policy("my_policy")

    expect_s3_class(policy1, "row_policy")
    expect_output(print(policy1), "<row_policy> my_policy")
    expect_s3_class(policy1$data, "tbl_sql")
    for (i in c("commands", "user", "existing_rows", "new_rows", "type")) {
      expect_null(policy1[[i]])
    }
  })
})
