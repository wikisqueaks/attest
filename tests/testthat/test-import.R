test_that("att_import generates a get-data.R script", {
  ts <- create_test_store()
  old_store <- getOption("attest.store")
  withr::defer(options(attest.store = old_store))
  att_store(ts$store)

  manifest_path <- file.path(ts$store, "manifest.json")
  att_export(path = manifest_path)

  new_store <- withr::local_tempdir()
  script_path <- file.path(new_store, "get-data.R")

  # Import into the same store so it skips (no network needed)
  att_import(
    path = manifest_path,
    script = script_path,
    store = ts$store
  )

  expect_true(file.exists(script_path))

  script_text <- readLines(script_path)
  expect_true(any(grepl("library\\(attest\\)", script_text)))
  expect_true(any(grepl("att_source", script_text)))
  expect_true(any(grepl("att_download", script_text)))
  expect_true(any(grepl("test-source", script_text)))
})

test_that("att_import registers local sources from manifest", {
  store <- withr::local_tempdir()
  old_store <- getOption("attest.store")
  withr::defer(options(attest.store = old_store))
  att_store(store)

  local_file <- file.path(withr::local_tempdir(), "data.csv")
  writeLines("a,b\n1,2", local_file)

  src <- att_source(
    name = "local-import-test",
    data_paths = c("data.csv" = local_file),
    title = "Import Test"
  )
  att_register(src, store = store)

  manifest_path <- file.path(store, "manifest.json")
  att_export(path = manifest_path, store = store)

  # Import into a new store
  new_store <- withr::local_tempdir()
  script_path <- file.path(new_store, "get-data.R")

  att_import(
    path = manifest_path,
    script = script_path,
    store = new_store
  )

  new_prov <- file.path(
    new_store, "local-import-test", "_attest", "provenance.json"
  )
  expect_true(file.exists(new_prov))

  # Script should contain att_register
  script_text <- readLines(script_path)
  expect_true(any(grepl("att_register", script_text)))
})

test_that("att_import skips existing sources", {
  ts <- create_test_store()
  old_store <- getOption("attest.store")
  withr::defer(options(attest.store = old_store))
  att_store(ts$store)

  manifest_path <- file.path(ts$store, "manifest.json")
  att_export(path = manifest_path)

  script_path <- file.path(ts$store, "get-data.R")

  # Import into the same store — should skip
  result <- att_import(
    path = manifest_path,
    script = script_path,
    store = ts$store
  )

  expect_equal(result, script_path)
})

test_that("att_import fails gracefully for missing local paths", {
  store <- withr::local_tempdir()

  manifest <- list(
    manifest_type = "attest_manifest",
    generated = "2026-01-01T00:00:00+0000",
    attest_version = "0.3.1",
    sources = list(
      list(
        name = "missing-files",
        origin = "local",
        data_paths = list("data.csv" = "/nonexistent/path/data.csv"),
        metadata = list(title = "Missing Data")
      )
    )
  )

  manifest_path <- file.path(store, "manifest.json")
  dir.create(store, showWarnings = FALSE)
  jsonlite::write_json(
    manifest, manifest_path,
    pretty = TRUE, auto_unbox = TRUE
  )

  script_path <- file.path(store, "get-data.R")
  att_import(
    path = manifest_path,
    script = script_path,
    store = store
  )

  # Script should still be generated even though execution failed
  expect_true(file.exists(script_path))
})

test_that("att_import validates manifest type", {
  store <- withr::local_tempdir()
  bad_file <- file.path(store, "bad.json")
  dir.create(store, showWarnings = FALSE)
  jsonlite::write_json(
    list(not_a_manifest = TRUE), bad_file,
    auto_unbox = TRUE
  )

  expect_snapshot(att_import(bad_file), error = TRUE)
})

test_that("att_import errors on missing file", {
  expect_snapshot(att_import("/nonexistent/manifest.json"), error = TRUE)
})

test_that("att_import handles empty manifest", {
  store <- withr::local_tempdir()
  manifest_path <- file.path(store, "empty.json")
  dir.create(store, showWarnings = FALSE)
  jsonlite::write_json(
    list(manifest_type = "attest_manifest", sources = list()),
    manifest_path,
    auto_unbox = TRUE
  )

  result <- att_import(manifest_path, store = store)
  expect_equal(result, "get-data.R")
})

test_that("att_import script includes classify for archive sources", {
  store <- withr::local_tempdir()

  manifest <- list(
    manifest_type = "attest_manifest",
    generated = "2026-01-01T00:00:00+0000",
    attest_version = "0.3.1",
    sources = list(
      list(
        name = "archive-test",
        origin = "remote",
        landing_url = "https://example.com",
        data_urls = list("data.zip" = "https://example.com/data.zip"),
        metadata = list(title = "Archive Test"),
        classify = list(
          data = list(".shp", ".dbf"),
          metadata = list(".pdf")
        )
      )
    )
  )

  manifest_path <- file.path(store, "manifest.json")
  dir.create(store, showWarnings = FALSE)
  jsonlite::write_json(
    manifest, manifest_path,
    pretty = TRUE, auto_unbox = TRUE
  )

  script_path <- file.path(store, "get-data.R")

  # Will fail on download (fake URL) but script should still be written
  att_import(
    path = manifest_path,
    script = script_path,
    store = store
  )

  script_text <- readLines(script_path)
  expect_true(any(grepl("classify", script_text)))
})
