test_that("acq_download rejects non-acq_source input", {
  expect_error(acq_download("not a source"), "acq_source")
})

test_that("acq_download creates correct directory structure", {
  skip_if_offline()
  store <- withr::local_tempdir()
  old_store <- getOption("acquire.store")
  withr::defer(options(acquire.store = old_store))
  acq_store(store)

  src <- acq_source(
    name = "httpbin-test",
    landing_url = "https://httpbin.org",
    data_urls = c("test.json" = "https://httpbin.org/json"),
    title = "httpbin test"
  )

  acq_download(src, cite = FALSE)

  # Data file at root
  expect_true(file.exists(file.path(store, "httpbin-test", "test.json")))
  # Provenance in _acquire/
  expect_true(file.exists(file.path(store, "httpbin-test", "_acquire", "provenance.json")))
})

test_that("acq_download writes valid provenance.json", {
  skip_if_offline()
  store <- withr::local_tempdir()
  old_store <- getOption("acquire.store")
  withr::defer(options(acquire.store = old_store))
  acq_store(store)

  src <- acq_source(
    name = "httpbin-prov",
    data_urls = c("test.json" = "https://httpbin.org/json"),
    title = "httpbin provenance test"
  )

  acq_download(src, cite = FALSE)

  prov <- acq_read_provenance(src, store = store)
  expect_equal(prov$name, "httpbin-prov")
  expect_equal(length(prov$files), 1)
  expect_false(is.null(prov$files[["test.json"]]$sha256))
  expect_equal(nchar(prov$files[["test.json"]]$sha256), 64)
  expect_equal(prov$files[["test.json"]]$location, "root")
})

test_that("acq_download puts metadata files in metadata/", {
  skip_if_offline()
  store <- withr::local_tempdir()
  old_store <- getOption("acquire.store")
  withr::defer(options(acquire.store = old_store))
  acq_store(store)

  src <- acq_source(
    name = "httpbin-meta",
    data_urls = c("data.json" = "https://httpbin.org/json"),
    metadata_urls = c("meta.json" = "https://httpbin.org/get"),
    title = "httpbin metadata test"
  )

  acq_download(src, cite = FALSE)

  expect_true(file.exists(file.path(store, "httpbin-meta", "data.json")))
  expect_true(file.exists(file.path(store, "httpbin-meta", "metadata", "meta.json")))

  prov <- acq_read_provenance(src, store = store)
  expect_equal(prov$files[["data.json"]]$location, "root")
  expect_equal(prov$files[["meta.json"]]$location, "metadata")
})

test_that("acq_download refuses to re-download existing source", {
  skip_if_offline()
  store <- withr::local_tempdir()
  old_store <- getOption("acquire.store")
  withr::defer(options(acquire.store = old_store))
  acq_store(store)

  src <- acq_source(
    name = "httpbin-skip",
    data_urls = c("test.json" = "https://httpbin.org/json"),
    title = "skip test"
  )

  acq_download(src, cite = FALSE)

  # Second download should error and recommend acq_refresh()
  expect_error(acq_download(src, cite = FALSE), "acq_refresh")
})

test_that("acq_download auto-generates citation by default", {
  skip_if_offline()
  store <- withr::local_tempdir()
  old_store <- getOption("acquire.store")
  withr::defer(options(acquire.store = old_store))
  acq_store(store)

  src <- acq_source(
    name = "httpbin-cite",
    data_urls = c("test.json" = "https://httpbin.org/json"),
    title = "Citation Test",
    publisher = "httpbin",
    year = "2025"
  )

  acq_download(src)

  bib_path <- file.path(store, "data-sources.bib")
  expect_true(file.exists(bib_path))
  content <- readLines(bib_path)
  expect_true(any(grepl("Citation Test", content)))
})
