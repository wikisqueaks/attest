# Helper: create a minimal link provenance store without network access
create_link_store <- function() {
  store <- withr::local_tempdir(.local_envir = parent.frame())
  name <- "link-source"
  source_dir <- file.path(store, name)
  attest_dir <- file.path(source_dir, "_attest")
  dir.create(attest_dir, recursive = TRUE)
  dir.create(file.path(source_dir, "metadata"), recursive = TRUE)

  writeLines(
    c("[InternetShortcut]", "URL=https://example.com/api"),
    file.path(source_dir, paste0(name, ".url"))
  )

  prov <- list(
    name = name,
    dir_name = name,
    origin = "link",
    data_urls = list("https://example.com/api"),
    metadata_urls = list(),
    landing_url = "https://example.com/",
    metadata = list(title = "Test API", publisher = "Test Org", year = "2026"),
    endpoint = list(
      linked = "2026-04-16T08:39:42Z",
      http_status = 200L,
      http_content_type = "application/json"
    ),
    files = list(),
    created = "2026-04-16T08:39:42Z",
    last_updated = "2026-04-16T08:39:42Z",
    attest_version = "0.3.1"
  )

  jsonlite::write_json(
    prov,
    file.path(attest_dir, "provenance.json"),
    pretty = TRUE, auto_unbox = TRUE
  )

  list(store = store, name = name)
}


# Input validation -----------------------------------------------------------

test_that("att_link rejects non-att_source input", {
  expect_error(att_link("not a source"), "att_source")
})

test_that("att_link rejects sources with local paths", {
  src <- att_source(
    name = "local-src",
    data_paths = c("data.csv" = "~/data/file.csv")
  )
  expect_error(att_link(src), "att_register")
})

test_that("att_link rejects sources with no URLs", {
  src <- att_source(name = "no-urls")
  expect_error(att_link(src), "no URLs")
})


# Directory structure and provenance (offline-compatible) --------------------
# .invalid TLD is reserved by RFC 2606: DNS lookup fails immediately,
# so the HEAD request errors and att_link() continues gracefully.

test_that("att_link creates correct directory structure", {
  store <- withr::local_tempdir()

  src <- att_source(
    name = "test-link",
    data_urls = "https://example.invalid/api/data"
  )

  att_link(src, store = store, cite = FALSE)

  expect_true(dir.exists(file.path(store, "test-link")))
  expect_true(dir.exists(file.path(store, "test-link", "_attest")))
  expect_true(
    file.exists(file.path(store, "test-link", "_attest", "provenance.json"))
  )
})

test_that("att_link writes .url Internet Shortcut file", {
  store <- withr::local_tempdir()

  src <- att_source(
    name = "test-link",
    data_urls = "https://example.invalid/api/data"
  )

  att_link(src, store = store, cite = FALSE)

  url_file <- file.path(store, "test-link", "test-link.url")
  expect_true(file.exists(url_file))

  content <- readLines(url_file)
  expect_equal(content[1], "[InternetShortcut]")
  expect_equal(content[2], "URL=https://example.invalid/api/data")
})

test_that("att_link uses landing_url for .url file when no data_urls set", {
  store <- withr::local_tempdir()

  src <- att_source(
    name = "landing-only",
    landing_url = "https://example.invalid/portal"
  )

  att_link(src, store = store, cite = FALSE)

  content <- readLines(file.path(store, "landing-only", "landing-only.url"))
  expect_equal(content[2], "URL=https://example.invalid/portal")
})

test_that("att_link writes provenance.json with origin = 'link'", {
  store <- withr::local_tempdir()

  src <- att_source(
    name = "test-link",
    data_urls = "https://example.invalid/api/data",
    landing_url = "https://example.invalid/",
    title = "Test API",
    publisher = "Test Org",
    year = "2026"
  )

  att_link(src, store = store, cite = FALSE)

  prov <- att_read_provenance(src, store = store)
  expect_equal(prov$origin, "link")
  expect_equal(prov$name, "test-link")
  expect_equal(prov$data_urls[[1]], "https://example.invalid/api/data")
  expect_equal(prov$metadata$title, "Test API")
  expect_false(is.null(prov$endpoint$linked))
})

test_that("att_link refuses to re-link existing source", {
  store <- withr::local_tempdir()

  src <- att_source(
    name = "test-link",
    data_urls = "https://example.invalid/api/data"
  )

  att_link(src, store = store, cite = FALSE)
  expect_error(att_link(src, store = store, cite = FALSE), "att_update")
})


# Online tests ---------------------------------------------------------------

test_that("att_link records HTTP metadata from HEAD request", {
  skip_if_offline()
  store <- withr::local_tempdir()

  src <- att_source(
    name = "httpbin-link",
    data_urls = "https://httpbin.org/json",
    title = "httpbin link test"
  )

  att_link(src, store = store, cite = FALSE)

  prov <- att_read_provenance(src, store = store)
  expect_equal(prov$origin, "link")
  expect_false(is.null(prov$endpoint$http_status))
  expect_equal(prov$endpoint$http_status, 200L)
})

test_that("att_link auto-generates citation by default", {
  skip_if_offline()
  store <- withr::local_tempdir()

  src <- att_source(
    name = "httpbin-link-cite",
    data_urls = "https://httpbin.org/json",
    title = "Link Citation Test",
    publisher = "httpbin",
    year = "2025"
  )

  att_link(src, store = store)

  bib_path <- file.path(store, "data-sources.bib")
  expect_true(file.exists(bib_path))
  expect_true(any(grepl("Link Citation Test", readLines(bib_path))))
})


# Downstream dispatch --------------------------------------------------------

test_that("att_verify skips gracefully for link sources", {
  fx <- create_link_store()
  result <- att_verify(fx$name, store = fx$store)
  expect_equal(result$all_ok, TRUE)
  expect_equal(length(result$files), 0)
})

test_that("att_refresh aborts for link sources", {
  fx <- create_link_store()
  expect_error(att_refresh(fx$name, store = fx$store), "att_check")
})

test_that("att_compare aborts for link sources", {
  fx <- create_link_store()
  expect_error(att_compare(fx$name, store = fx$store), "att_check")
})


# Export / import ------------------------------------------------------------

test_that("att_export includes link sources with origin = 'link'", {
  fx <- create_link_store()

  manifest_path <- file.path(fx$store, "manifest.json")
  manifest <- att_export(path = manifest_path, store = fx$store)

  entry <- manifest$sources[[1]]
  expect_equal(entry$origin, "link")
  expect_equal(entry$data_urls[[1]], "https://example.com/api")
})

test_that("att_import generates att_link() call for link sources", {
  fx <- create_link_store()

  manifest_path <- file.path(fx$store, "manifest.json")
  att_export(path = manifest_path, store = fx$store)

  # Import into a fresh store
  store2 <- withr::local_tempdir()
  script_path <- file.path(store2, "get-data.R")
  att_import(path = manifest_path, script = script_path, store = store2)

  script <- readLines(script_path)
  expect_true(any(grepl("att_link", script)))
})
