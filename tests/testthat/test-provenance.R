test_that("att_read_provenance returns provenance record", {
  fix <- create_test_store()
  prov <- att_read_provenance(fix$source, store = fix$store)

  expect_type(prov, "list")
  expect_equal(prov$name, "test-source")
  expect_equal(prov$landing_url, "https://example.com/data")
  expect_equal(prov$metadata$title, "Test Dataset")
  expect_equal(length(prov$files), 2)
  expect_equal(prov$files[["data.csv"]]$sha256, fix$data_hash)
  expect_equal(prov$files[["codebook.txt"]]$location, "metadata")
})

test_that("att_read_provenance accepts string name", {
  fix <- create_test_store()
  prov <- att_read_provenance("test-source", store = fix$store)
  expect_equal(prov$name, "test-source")
})

test_that("att_read_provenance errors for unknown source", {
  store <- withr::local_tempdir()
  expect_error(att_read_provenance("nonexistent", store = store))
})

test_that("att_status finds sources by scanning _acquire dirs", {
  fix <- create_test_store()
  result <- att_status(store = fix$store)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)
  expect_equal(result$name, "test-source")
  expect_equal(result$n_files, 2)
  expect_true(result$total_bytes > 0)
  expect_true("origin" %in% names(result))
  # Legacy provenance without explicit origin defaults to "remote"
  expect_equal(result$origin, "remote")
})

test_that("att_status shows origin for local sources", {
  store <- withr::local_tempdir()
  old_store <- getOption("attest.store")
  withr::defer(options(attest.store = old_store))
  att_store(store)

  ext_dir <- withr::local_tempdir()
  data_file <- file.path(ext_dir, "data.csv")
  writeLines("a\n1", data_file)

  src <- att_source(
    name = "local-status",
    data_paths = c("data.csv" = data_file),
    title = "Local Status Test"
  )

  att_register(src, cite = FALSE)

  result <- att_status(store = store)

  expect_equal(nrow(result), 1)
  expect_equal(result$origin, "local")
})

test_that("provenance_path resolves correctly", {
  expect_match(provenance_path("/store", "my-source"),
               file.path("/store", "my-source", "_acquire", "provenance.json"),
               fixed = TRUE)
})

test_that("resolve_file_path handles root and metadata locations", {
  root_info <- list(location = "root")
  meta_info <- list(location = "metadata")
  no_loc <- list()

  expect_match(resolve_file_path("/s", "src", "f.csv", root_info),
               file.path("/s", "src", "f.csv"), fixed = TRUE)
  expect_match(resolve_file_path("/s", "src", "f.pdf", meta_info),
               file.path("/s", "src", "metadata", "f.pdf"), fixed = TRUE)
  # Default to root when location is missing
  expect_match(resolve_file_path("/s", "src", "f.csv", no_loc),
               file.path("/s", "src", "f.csv"), fixed = TRUE)
})
