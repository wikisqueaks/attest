# -- att_add_metadata() tests --------------------------------------------------

test_that("att_add_metadata errors when no provenance record exists", {
  store <- withr::local_tempdir()
  expect_error(
    att_add_metadata("nonexistent", "https://example.com/meta.pdf", store = store),
    "No provenance record"
  )
})

test_that("att_add_metadata errors on invalid urls argument", {
  ts <- create_test_store()
  expect_error(
    att_add_metadata(ts$name, character(0), store = ts$store),
    "non-empty"
  )
  expect_error(
    att_add_metadata(ts$name, 42, store = ts$store),
    "non-empty"
  )
})

test_that("att_add_metadata errors when file already in provenance", {
  ts <- create_test_store()
  expect_error(
    att_add_metadata(
      ts$name,
      c("codebook.txt" = "https://example.com/codebook.txt"),
      store = ts$store
    ),
    "already in provenance"
  )
})

test_that("att_add_metadata downloads and records new metadata file", {
  skip_if_offline()
  ts <- create_test_store()

  result <- att_add_metadata(
    ts$name,
    c("robots.txt" = "https://httpbin.org/robots.txt"),
    store = ts$store
  )

  # File should exist on disk
  meta_path <- file.path(ts$source_dir, "metadata", "robots.txt")
  expect_true(file.exists(meta_path))

  # Provenance should include the new file
  prov <- jsonlite::read_json(provenance_path(ts$store, ts$name))
  expect_true("robots.txt" %in% names(prov$files))
  expect_equal(prov$files[["robots.txt"]]$location, "metadata")
  expect_false(is.null(prov$files[["robots.txt"]]$sha256))

  # Existing files should be untouched
  expect_equal(prov$files[["data.csv"]]$sha256, ts$data_hash)
  expect_equal(prov$files[["codebook.txt"]]$sha256, ts$meta_hash)
})

test_that("att_add_metadata accepts source object instead of name", {
  skip_if_offline()
  ts <- create_test_store()

  result <- att_add_metadata(
    ts$source,
    c("robots.txt" = "https://httpbin.org/robots.txt"),
    store = ts$store
  )

  prov <- jsonlite::read_json(provenance_path(ts$store, ts$name))
  expect_true("robots.txt" %in% names(prov$files))
})

test_that("att_add_metadata updates last_updated timestamp", {
  skip_if_offline()
  ts <- create_test_store()

  prov_before <- jsonlite::read_json(provenance_path(ts$store, ts$name))

  Sys.sleep(1)
  att_add_metadata(
    ts$name,
    c("robots.txt" = "https://httpbin.org/robots.txt"),
    store = ts$store
  )

  prov_after <- jsonlite::read_json(provenance_path(ts$store, ts$name))
  expect_true(prov_after$last_updated > prov_before$last_updated)
})

test_that("att_add_metadata derives filename from URL when unnamed", {
  skip_if_offline()
  ts <- create_test_store()

  att_add_metadata(
    ts$name,
    "https://httpbin.org/robots.txt",
    store = ts$store
  )

  prov <- jsonlite::read_json(provenance_path(ts$store, ts$name))
  expect_true("robots.txt" %in% names(prov$files))
})

test_that("att_add_metadata handles multiple URLs", {
  skip_if_offline()
  ts <- create_test_store()

  att_add_metadata(
    ts$name,
    c(
      "robots.txt" = "https://httpbin.org/robots.txt",
      "test.json"  = "https://httpbin.org/json"
    ),
    store = ts$store
  )

  prov <- jsonlite::read_json(provenance_path(ts$store, ts$name))
  expect_true("robots.txt" %in% names(prov$files))
  expect_true("test.json" %in% names(prov$files))
  expect_equal(prov$files[["robots.txt"]]$location, "metadata")
  expect_equal(prov$files[["test.json"]]$location, "metadata")
})
