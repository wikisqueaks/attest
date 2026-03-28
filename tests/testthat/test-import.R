test_that("att_import registers local sources from manifest", {
  # Create a source store, export it, then import into a new store
  store <- withr::local_tempdir()
  old_store <- getOption("attest.store")
  withr::defer(options(attest.store = old_store))
  att_store(store)

  # Create a local data file to register
  local_file <- file.path(withr::local_tempdir(), "data.csv")
  writeLines("a,b\n1,2", local_file)

  src <- att_source(
    name = "local-import-test",
    data_paths = c("data.csv" = local_file),
    title = "Import Test"
  )
  att_register(src, store = store)

  manifest <- att_export(store = store)

  # Import into a new store
  new_store <- withr::local_tempdir()
  manifest_path <- file.path(store, "attest-manifest.json")

  result <- att_import(manifest_path, store = new_store)

  expect_equal(nrow(result), 1)
  expect_equal(result$source, "local-import-test")
  expect_equal(result$status, "registered")

  # Verify the new store has the source
  new_prov_path <- file.path(
    new_store, "local-import-test", "_attest", "provenance.json"
  )
  expect_true(file.exists(new_prov_path))
})

test_that("att_import skips existing sources", {
  ts <- create_test_store()
  old_store <- getOption("attest.store")
  withr::defer(options(attest.store = old_store))
  att_store(ts$store)

  # Create a manifest pointing at the same store
  manifest <- att_export(store = ts$store)
  manifest_path <- file.path(ts$store, "attest-manifest.json")

  # Import into the same store — should skip
  result <- att_import(manifest_path, store = ts$store)

  expect_equal(nrow(result), 1)
  expect_equal(result$status, "skipped")
})

test_that("att_import fails gracefully for missing local paths", {
  store <- withr::local_tempdir()

  # Write a manifest with a nonexistent local path
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
  jsonlite::write_json(
    manifest, manifest_path,
    pretty = TRUE, auto_unbox = TRUE
  )

  result <- att_import(manifest_path, store = store)

  expect_equal(nrow(result), 1)
  expect_equal(result$status, "failed")
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
  expect_equal(nrow(result), 0)
})
