# -- acq_check() tests for local sources --------------------------------------

test_that("acq_check reports unchanged for unmodified local source", {
  store <- withr::local_tempdir()
  old_store <- getOption("acquire.store")
  withr::defer(options(acquire.store = old_store))
  acq_store(store)

  ext_dir <- withr::local_tempdir()
  data_file <- file.path(ext_dir, "data.csv")
  writeLines("x,y\n1,2", data_file)

  src <- acq_source(
    name = "check-unchanged",
    data_paths = c("data.csv" = data_file),
    title = "check unchanged test"
  )

  acq_register(src, cite = FALSE)

  result <- acq_check(src, store = store)

  expect_s3_class(result, "acq_check")
  expect_equal(result$files[["data.csv"]]$status, "unchanged")
})

test_that("acq_check detects size change in local source", {
  store <- withr::local_tempdir()
  old_store <- getOption("acquire.store")
  withr::defer(options(acquire.store = old_store))
  acq_store(store)

  ext_dir <- withr::local_tempdir()
  data_file <- file.path(ext_dir, "data.csv")
  writeLines("x,y\n1,2", data_file)

  src <- acq_source(
    name = "check-size",
    data_paths = c("data.csv" = data_file),
    title = "check size test"
  )

  acq_register(src, cite = FALSE)

  # Modify the original to change size
  writeLines("x,y\n1,2\n3,4\n5,6", data_file)

  result <- acq_check(src, store = store)

  expect_equal(result$files[["data.csv"]]$status, "possibly_changed")
  expect_true("size changed" %in% result$files[["data.csv"]]$changes)
})

test_that("acq_check detects mtime change in local source", {
  store <- withr::local_tempdir()
  old_store <- getOption("acquire.store")
  withr::defer(options(acquire.store = old_store))
  acq_store(store)

  ext_dir <- withr::local_tempdir()
  data_file <- file.path(ext_dir, "data.csv")
  writeLines("x,y\n1,2", data_file)

  src <- acq_source(
    name = "check-mtime",
    data_paths = c("data.csv" = data_file),
    title = "check mtime test"
  )

  acq_register(src, cite = FALSE)

  # Touch the file to change mtime without changing content/size
  Sys.sleep(1.1)
  writeLines("x,y\n1,2", data_file)

  result <- acq_check(src, store = store)

  expect_equal(result$files[["data.csv"]]$status, "possibly_changed")
  expect_true("modified time changed" %in% result$files[["data.csv"]]$changes)
})

test_that("acq_check reports source_missing for deleted local source", {
  store <- withr::local_tempdir()
  old_store <- getOption("acquire.store")
  withr::defer(options(acquire.store = old_store))
  acq_store(store)

  ext_dir <- withr::local_tempdir()
  data_file <- file.path(ext_dir, "data.csv")
  writeLines("a\n1", data_file)

  src <- acq_source(
    name = "check-missing",
    data_paths = c("data.csv" = data_file),
    title = "check missing test"
  )

  acq_register(src, cite = FALSE)
  file.remove(data_file)

  result <- acq_check(src, store = store)

  expect_equal(result$files[["data.csv"]]$status, "source_missing")
})

test_that("acq_check handles mixed local files", {
  store <- withr::local_tempdir()
  old_store <- getOption("acquire.store")
  withr::defer(options(acquire.store = old_store))
  acq_store(store)

  ext_dir <- withr::local_tempdir()
  data_file <- file.path(ext_dir, "data.csv")
  meta_file <- file.path(ext_dir, "codebook.txt")
  writeLines("x\n1", data_file)
  writeLines("x: numeric", meta_file)

  src <- acq_source(
    name = "check-mixed",
    data_paths = c("data.csv" = data_file),
    metadata_paths = c("codebook.txt" = meta_file),
    title = "check mixed test"
  )

  acq_register(src, cite = FALSE)

  # Modify only the metadata file
  writeLines("x: numeric\ny: character", meta_file)

  result <- acq_check(src, store = store)

  expect_equal(result$files[["data.csv"]]$status, "unchanged")
  expect_equal(result$files[["codebook.txt"]]$status, "possibly_changed")
})
