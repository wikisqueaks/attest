# -- acq_archive() tests -------------------------------------------------------

test_that("acq_archive preserves full directory structure", {
  env <- create_test_store()

  archive_path <- acq_archive(env$name, store = env$store)

  expect_true(dir.exists(archive_path))
  expect_true(file.exists(file.path(archive_path, "data.csv")))
  expect_true(dir.exists(file.path(archive_path, "metadata")))
  expect_true(file.exists(file.path(archive_path, "metadata", "codebook.txt")))
  expect_true(dir.exists(file.path(archive_path, "_acquire")))
  expect_true(file.exists(
    file.path(archive_path, "_acquire", "provenance.json")
  ))
})

test_that("acq_archive errors for nonexistent source", {
  store <- withr::local_tempdir()
  expect_error(acq_archive("nope", store = store), "not found")
})

test_that("acq_archive does not include itself in the archive", {
  env <- create_test_store()

  # Create two archives
  acq_archive(env$name, store = env$store)
  archive_path <- acq_archive(env$name, store = env$store)

  # The second archive should not contain an archive/ subdirectory
  expect_false(dir.exists(file.path(archive_path, "archive")))
})


# -- acq_refresh() tests ------------------------------------------------------

test_that("acq_refresh errors for unknown source", {
  store <- withr::local_tempdir()
  expect_error(acq_refresh("nope", store = store), "No provenance record")
})

test_that("acq_refresh detects no changes", {
  skip_if_offline()
  store <- withr::local_tempdir()
  old_store <- getOption("acquire.store")
  withr::defer(options(acquire.store = old_store))
  acq_store(store)

  src <- acq_source(
    name = "httpbin-refresh-unchanged",
    data_urls = c("test.json" = "https://httpbin.org/json"),
    title = "refresh unchanged test"
  )

  acq_download(src, cite = FALSE)

  result <- acq_refresh(src, store = store, archive = FALSE)

  expect_s3_class(result, "data.frame")
  expect_true(all(result$status == "unchanged"))
})

test_that("acq_refresh detects changes and archives", {
  skip_if_offline()
  store <- withr::local_tempdir()
  old_store <- getOption("acquire.store")
  withr::defer(options(acquire.store = old_store))
  acq_store(store)

  src <- acq_source(
    name = "httpbin-refresh-changed",
    data_urls = c("test.json" = "https://httpbin.org/json"),
    title = "refresh changed test"
  )

  acq_download(src, cite = FALSE)

  # Tamper with the recorded hash in provenance to simulate a remote change
  prov_path <- file.path(
    store, "httpbin-refresh-changed", "_acquire", "provenance.json"
  )
  prov <- jsonlite::read_json(prov_path)
  prov$files[["test.json"]]$sha256 <- "0000000000000000000000000000000000000000000000000000000000000000"
  jsonlite::write_json(prov, prov_path, pretty = TRUE, auto_unbox = TRUE)

  result <- acq_refresh(src, store = store, archive = TRUE)

  expect_s3_class(result, "data.frame")
  expect_true("changed" %in% result$status)

  # Archive should have been created
  archive_root <- file.path(store, "httpbin-refresh-changed", "archive")
  expect_true(dir.exists(archive_root))
  archives <- list.dirs(archive_root, recursive = FALSE)
  expect_equal(length(archives), 1)

  # Archived provenance should contain the fake hash
  archived_prov <- jsonlite::read_json(
    file.path(archives[1], "_acquire", "provenance.json")
  )
  expect_equal(
    archived_prov$files[["test.json"]]$sha256,
    "0000000000000000000000000000000000000000000000000000000000000000"
  )
})

test_that("acq_refresh preserves created timestamp", {
  skip_if_offline()
  store <- withr::local_tempdir()
  old_store <- getOption("acquire.store")
  withr::defer(options(acquire.store = old_store))
  acq_store(store)

  src <- acq_source(
    name = "httpbin-refresh-ts",
    data_urls = c("test.json" = "https://httpbin.org/json"),
    title = "timestamp test"
  )

  acq_download(src, cite = FALSE)
  prov_before <- acq_read_provenance(src, store = store)

  # Tamper with the recorded hash and set an old last_updated to force a

  # detectable change in both hash and timestamp
  prov_path <- file.path(
    store, "httpbin-refresh-ts", "_acquire", "provenance.json"
  )
  prov_raw <- jsonlite::read_json(prov_path)
  prov_raw$files[["test.json"]]$sha256 <- "0000000000000000000000000000000000000000000000000000000000000000"
  prov_raw$last_updated <- "2020-01-01T00:00:00+0000"
  jsonlite::write_json(prov_raw, prov_path, pretty = TRUE, auto_unbox = TRUE)

  acq_refresh(src, store = store, archive = FALSE)
  prov_after <- acq_read_provenance(src, store = store)

  expect_equal(prov_after$created, prov_before$created)
  expect_false(identical(prov_after$last_updated, "2020-01-01T00:00:00+0000"))
})

test_that("acq_refresh accepts string source names", {
  skip_if_offline()
  store <- withr::local_tempdir()
  old_store <- getOption("acquire.store")
  withr::defer(options(acquire.store = old_store))
  acq_store(store)

  src <- acq_source(
    name = "httpbin-refresh-str",
    data_urls = c("test.json" = "https://httpbin.org/json"),
    title = "string name test"
  )

  acq_download(src, cite = FALSE)

  # Use string name instead of source object
  result <- acq_refresh("httpbin-refresh-str", store = store, archive = FALSE)
  expect_s3_class(result, "data.frame")
})

test_that("acq_refresh returns correct summary columns", {
  skip_if_offline()
  store <- withr::local_tempdir()
  old_store <- getOption("acquire.store")
  withr::defer(options(acquire.store = old_store))
  acq_store(store)

  src <- acq_source(
    name = "httpbin-refresh-cols",
    data_urls = c("test.json" = "https://httpbin.org/json"),
    title = "columns test"
  )

  acq_download(src, cite = FALSE)
  result <- acq_refresh(src, store = store, archive = FALSE)

  expect_named(
    result,
    c("file", "status", "old_hash", "new_hash", "old_size", "new_size")
  )
})
