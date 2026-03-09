test_that("att_download rejects non-att_source input", {
  expect_error(att_download("not a source"), "att_source")
})

test_that("att_download rejects sources with local paths", {
  src <- att_source(
    name = "local-src",
    data_paths = c("data.csv" = "~/data/file.csv"),
    title = "Local Source"
  )
  expect_error(att_download(src), "att_register")
})

test_that("att_download creates correct directory structure", {
  skip_if_offline()
  store <- withr::local_tempdir()
  old_store <- getOption("attest.store")
  withr::defer(options(attest.store = old_store))
  att_store(store)

  src <- att_source(
    name = "httpbin-test",
    landing_url = "https://httpbin.org",
    data_urls = c("test.json" = "https://httpbin.org/json"),
    title = "httpbin test"
  )

  att_download(src, cite = FALSE)

  # Data file at root
  expect_true(file.exists(file.path(store, "httpbin-test", "test.json")))
  # Provenance in _acquire/
  expect_true(file.exists(file.path(store, "httpbin-test", "_acquire", "provenance.json")))
})

test_that("att_download writes valid provenance.json", {
  skip_if_offline()
  store <- withr::local_tempdir()
  old_store <- getOption("attest.store")
  withr::defer(options(attest.store = old_store))
  att_store(store)

  src <- att_source(
    name = "httpbin-prov",
    data_urls = c("test.json" = "https://httpbin.org/json"),
    title = "httpbin provenance test"
  )

  att_download(src, cite = FALSE)

  prov <- att_read_provenance(src, store = store)
  expect_equal(prov$name, "httpbin-prov")
  expect_equal(length(prov$files), 1)
  expect_false(is.null(prov$files[["test.json"]]$sha256))
  expect_equal(nchar(prov$files[["test.json"]]$sha256), 64)
  expect_equal(prov$files[["test.json"]]$location, "root")
})

test_that("att_download puts metadata files in metadata/", {
  skip_if_offline()
  store <- withr::local_tempdir()
  old_store <- getOption("attest.store")
  withr::defer(options(attest.store = old_store))
  att_store(store)

  src <- att_source(
    name = "httpbin-meta",
    data_urls = c("data.json" = "https://httpbin.org/json"),
    metadata_urls = c("meta.json" = "https://httpbin.org/get"),
    title = "httpbin metadata test"
  )

  att_download(src, cite = FALSE)

  expect_true(file.exists(file.path(store, "httpbin-meta", "data.json")))
  expect_true(file.exists(file.path(store, "httpbin-meta", "metadata", "meta.json")))

  prov <- att_read_provenance(src, store = store)
  expect_equal(prov$files[["data.json"]]$location, "root")
  expect_equal(prov$files[["meta.json"]]$location, "metadata")
})

test_that("att_download refuses to re-download existing source", {
  skip_if_offline()
  store <- withr::local_tempdir()
  old_store <- getOption("attest.store")
  withr::defer(options(attest.store = old_store))
  att_store(store)

  src <- att_source(
    name = "httpbin-skip",
    data_urls = c("test.json" = "https://httpbin.org/json"),
    title = "skip test"
  )

  att_download(src, cite = FALSE)

  # Second download should error and recommend att_refresh()
  expect_error(att_download(src, cite = FALSE), "att_refresh")
})

test_that("att_download auto-generates citation by default", {
  skip_if_offline()
  store <- withr::local_tempdir()
  old_store <- getOption("attest.store")
  withr::defer(options(attest.store = old_store))
  att_store(store)

  src <- att_source(
    name = "httpbin-cite",
    data_urls = c("test.json" = "https://httpbin.org/json"),
    title = "Citation Test",
    publisher = "httpbin",
    year = "2025"
  )

  att_download(src)

  bib_path <- file.path(store, "data-sources.bib")
  expect_true(file.exists(bib_path))
  content <- readLines(bib_path)
  expect_true(any(grepl("Citation Test", content)))
})
