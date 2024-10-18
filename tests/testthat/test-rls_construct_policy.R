test_that("rls_construct_policy", {
  x <- rls_construct_policy(
    name = "hide_confidential",
    table = "sometable",
    check = "confidential BOOLEAN",
    using = "confidential = false"
  )
  expect_s3_class(x, "rls_policy")
  expect_type(unclass(x), "list")
  expect_equal(x$name, "hide_confidential")
})
