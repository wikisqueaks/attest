test_that("att_store get/set works", {
  old <- getOption("attest.store")
  withr::defer(options(attest.store = old))

  att_store("/tmp/test-store")
  expect_equal(att_store(), normalizePath("/tmp/test-store", mustWork = FALSE))
})

test_that("att_store defaults to data/raw", {
  old <- getOption("attest.store")
  withr::defer(options(attest.store = old))
  options(attest.store = NULL)

  withr::with_envvar(c(ATTEST_STORE = ""), {
    expect_equal(att_store(), file.path(getwd(), "data", "raw"))
  })
})

test_that("att_status returns empty data frame for empty store", {
  store <- withr::local_tempdir()
  result <- att_status(store = store)
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
})

test_that("att_status returns empty data frame for nonexistent store", {
  result <- att_status(store = "/nonexistent/path")
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
})
