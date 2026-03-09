# -- att_compare() tests for local sources ------------------------------------

test_that("att_compare reports match for unchanged local source", {
  store <- withr::local_tempdir()
  old_store <- getOption("attest.store")
  withr::defer(options(attest.store = old_store))
  att_store(store)

  ext_dir <- withr::local_tempdir()
  data_file <- file.path(ext_dir, "data.csv")
  writeLines("x,y\n1,2", data_file)

  src <- att_source(
    name = "compare-match",
    data_paths = c("data.csv" = data_file),
    title = "compare match test"
  )

  att_register(src, cite = FALSE)

  result <- att_compare(src, store = store)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)
  expect_equal(result$status, "match")
  expect_equal(result$recorded_hash, result$source_hash)
})

test_that("att_compare detects changed local source", {
  store <- withr::local_tempdir()
  old_store <- getOption("attest.store")
  withr::defer(options(attest.store = old_store))
  att_store(store)

  ext_dir <- withr::local_tempdir()
  data_file <- file.path(ext_dir, "data.csv")
  writeLines("x,y\n1,2", data_file)

  src <- att_source(
    name = "compare-changed",
    data_paths = c("data.csv" = data_file),
    title = "compare changed test"
  )

  att_register(src, cite = FALSE)

  # Modify the original source file
  writeLines("x,y\n1,2\n3,4", data_file)

  result <- att_compare(src, store = store)

  expect_equal(result$status, "changed")
  expect_false(identical(result$recorded_hash, result$source_hash))
})

test_that("att_compare reports error when local source file is missing", {
  store <- withr::local_tempdir()
  old_store <- getOption("attest.store")
  withr::defer(options(attest.store = old_store))
  att_store(store)

  ext_dir <- withr::local_tempdir()
  data_file <- file.path(ext_dir, "data.csv")
  writeLines("a\n1", data_file)

  src <- att_source(
    name = "compare-missing",
    data_paths = c("data.csv" = data_file),
    title = "compare missing test"
  )

  att_register(src, cite = FALSE)

  # Delete the original
  file.remove(data_file)

  result <- att_compare(src, store = store)

  expect_equal(result$status, "error")
  expect_true(is.na(result$source_hash))
})

test_that("att_compare handles mixed statuses across multiple local files", {
  store <- withr::local_tempdir()
  old_store <- getOption("attest.store")
  withr::defer(options(attest.store = old_store))
  att_store(store)

  ext_dir <- withr::local_tempdir()
  data_file <- file.path(ext_dir, "data.csv")
  meta_file <- file.path(ext_dir, "codebook.txt")
  writeLines("x\n1", data_file)
  writeLines("x: numeric", meta_file)

  src <- att_source(
    name = "compare-mixed",
    data_paths = c("data.csv" = data_file),
    metadata_paths = c("codebook.txt" = meta_file),
    title = "compare mixed test"
  )

  att_register(src, cite = FALSE)

  # Modify only the metadata file
  writeLines("x: numeric\ny: character", meta_file)

  result <- att_compare(src, store = store)

  expect_equal(nrow(result), 2)
  data_row <- result[result$file == "data.csv", ]
  meta_row <- result[result$file == "codebook.txt", ]
  expect_equal(data_row$status, "match")
  expect_equal(meta_row$status, "changed")
})

test_that("att_compare returns correct columns", {
  store <- withr::local_tempdir()
  old_store <- getOption("attest.store")
  withr::defer(options(attest.store = old_store))
  att_store(store)

  ext_dir <- withr::local_tempdir()
  data_file <- file.path(ext_dir, "data.csv")
  writeLines("z\n9", data_file)

  src <- att_source(
    name = "compare-cols",
    data_paths = c("data.csv" = data_file),
    title = "compare cols test"
  )

  att_register(src, cite = FALSE)

  result <- att_compare(src, store = store)

  expect_named(result, c("file", "status", "recorded_hash", "source_hash"))
})

test_that("att_compare accepts string source names", {
  store <- withr::local_tempdir()
  old_store <- getOption("attest.store")
  withr::defer(options(attest.store = old_store))
  att_store(store)

  ext_dir <- withr::local_tempdir()
  data_file <- file.path(ext_dir, "data.csv")
  writeLines("w\n5", data_file)

  src <- att_source(
    name = "compare-string",
    data_paths = c("data.csv" = data_file),
    title = "compare string test"
  )

  att_register(src, cite = FALSE)

  result <- att_compare("compare-string", store = store)

  expect_s3_class(result, "data.frame")
  expect_equal(result$status, "match")
})
