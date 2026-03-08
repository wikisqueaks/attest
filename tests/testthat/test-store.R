test_that("acq_store get/set works", {
  old <- getOption("acquire.store")
  withr::defer(options(acquire.store = old))

  acq_store("/tmp/test-store")
  expect_equal(acq_store(), normalizePath("/tmp/test-store", mustWork = FALSE))
})

test_that("acq_store defaults to data/raw", {
  old <- getOption("acquire.store")
  withr::defer(options(acquire.store = old))
  options(acquire.store = NULL)

  withr::with_envvar(c(ACQUIRE_STORE = ""), {
    expect_equal(acq_store(), file.path(getwd(), "data", "raw"))
  })
})

test_that("acq_status returns empty data frame for empty store", {
  store <- withr::local_tempdir()
  result <- acq_status(store = store)
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
})

test_that("acq_status returns empty data frame for nonexistent store", {
  result <- acq_status(store = "/nonexistent/path")
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
})
