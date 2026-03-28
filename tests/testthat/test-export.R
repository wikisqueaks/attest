test_that("att_export creates a valid manifest from a single source", {
  ts <- create_test_store()
  old_store <- getOption("attest.store")
  withr::defer(options(attest.store = old_store))
  att_store(ts$store)

  manifest <- att_export()

  expect_equal(manifest$manifest_type, "attest_manifest")
  expect_equal(length(manifest$sources), 1)

  src <- manifest$sources[[1]]
  expect_equal(src$name, "test-source")
  expect_equal(src$origin, "remote")
  expect_equal(src$landing_url, "https://example.com/data")
  expect_equal(src$data_urls[["data.csv"]], "https://example.com/data.csv")
  expect_equal(
    src$metadata_urls[["codebook.txt"]],
    "https://example.com/codebook.txt"
  )
  expect_equal(src$metadata$title, "Test Dataset")
})

test_that("att_export writes a JSON file", {
  ts <- create_test_store()
  old_store <- getOption("attest.store")
  withr::defer(options(attest.store = old_store))
  att_store(ts$store)

  att_export()
  manifest_path <- file.path(ts$store, "attest-manifest.json")

  expect_true(file.exists(manifest_path))

  parsed <- jsonlite::read_json(manifest_path)
  expect_equal(parsed$manifest_type, "attest_manifest")
  expect_equal(length(parsed$sources), 1)
})

test_that("att_export writes to custom path", {
  ts <- create_test_store()
  old_store <- getOption("attest.store")
  withr::defer(options(attest.store = old_store))
  att_store(ts$store)

  custom_path <- file.path(ts$store, "custom", "manifest.json")
  att_export(path = custom_path)

  expect_true(file.exists(custom_path))
})

test_that("att_export handles local sources", {
  store <- withr::local_tempdir()
  old_store <- getOption("attest.store")
  withr::defer(options(attest.store = old_store))
  att_store(store)

  source_dir <- file.path(store, "local-data")
  attest_dir <- file.path(source_dir, "_attest")
  dir.create(attest_dir, recursive = TRUE)

  data_file <- file.path(source_dir, "data.csv")
  writeLines("x,y\n1,2", data_file)

  prov <- list(
    name = "local-data",
    dir_name = "local-data",
    origin = "local",
    landing_url = NULL,
    metadata = list(title = "Local Dataset"),
    files = list(
      "data.csv" = list(
        source_path = "/original/path/data.csv",
        registered = "2026-01-01T00:00:00+0000",
        size = file.size(data_file),
        sha256 = att_hash(data_file),
        location = "root"
      )
    ),
    created = "2026-01-01T00:00:00+0000",
    last_updated = "2026-01-01T00:00:00+0000"
  )

  jsonlite::write_json(
    prov,
    file.path(attest_dir, "provenance.json"),
    pretty = TRUE, auto_unbox = TRUE
  )

  manifest <- att_export()

  src <- manifest$sources[[1]]
  expect_equal(src$origin, "local")
  expect_equal(src$data_paths[["data.csv"]], "/original/path/data.csv")
  expect_null(src$data_urls)
})

test_that("att_export handles archive sources", {
  store <- withr::local_tempdir()
  old_store <- getOption("attest.store")
  withr::defer(options(attest.store = old_store))
  att_store(store)

  source_dir <- file.path(store, "archive-data")
  attest_dir <- file.path(source_dir, "_attest")
  metadata_dir <- file.path(source_dir, "metadata")
  dir.create(attest_dir, recursive = TRUE)
  dir.create(metadata_dir, recursive = TRUE)

  data_file <- file.path(source_dir, "shapes.shp")
  meta_file <- file.path(metadata_dir, "readme.pdf")
  writeLines("fake shapefile", data_file)
  writeLines("fake readme", meta_file)

  prov <- list(
    name = "archive-data",
    dir_name = "archive-data",
    origin = "remote",
    landing_url = "https://example.com/geodata",
    metadata = list(title = "Geo Data"),
    archives = list(
      "data.zip" = list(
        url = "https://example.com/data.zip",
        downloaded = "2026-01-01T00:00:00+0000",
        sha256 = "abc123",
        size = 9999
      )
    ),
    files = list(
      "shapes.shp" = list(
        extracted_from = "data.zip",
        size = file.size(data_file),
        sha256 = att_hash(data_file),
        location = "root"
      ),
      "readme.pdf" = list(
        extracted_from = "data.zip",
        size = file.size(meta_file),
        sha256 = att_hash(meta_file),
        location = "metadata"
      )
    ),
    created = "2026-01-01T00:00:00+0000",
    last_updated = "2026-01-01T00:00:00+0000"
  )

  jsonlite::write_json(
    prov,
    file.path(attest_dir, "provenance.json"),
    pretty = TRUE, auto_unbox = TRUE
  )

  manifest <- att_export()

  src <- manifest$sources[[1]]
  expect_equal(src$data_urls[["data.zip"]], "https://example.com/data.zip")
  expect_null(src$data_urls[["shapes.shp"]])
  expect_true(".pdf" %in% src$classify$metadata)
  expect_true(".shp" %in% src$classify$data)
})

test_that("att_export handles multiple sources", {
  store <- withr::local_tempdir()
  old_store <- getOption("attest.store")
  withr::defer(options(attest.store = old_store))
  att_store(store)

  for (sname in c("source-a", "source-b")) {
    source_dir <- file.path(store, sname)
    attest_dir <- file.path(source_dir, "_attest")
    dir.create(attest_dir, recursive = TRUE)

    data_file <- file.path(source_dir, "data.csv")
    writeLines("x,y\n1,2", data_file)

    prov <- list(
      name = sname,
      dir_name = sname,
      origin = "remote",
      landing_url = "https://example.com",
      metadata = list(title = sname),
      files = list(
        "data.csv" = list(
          url = paste0("https://example.com/", sname, "/data.csv"),
          downloaded = "2026-01-01T00:00:00+0000",
          size = file.size(data_file),
          sha256 = att_hash(data_file),
          location = "root"
        )
      ),
      created = "2026-01-01T00:00:00+0000",
      last_updated = "2026-01-01T00:00:00+0000"
    )

    jsonlite::write_json(
      prov,
      file.path(attest_dir, "provenance.json"),
      pretty = TRUE, auto_unbox = TRUE
    )
  }

  manifest <- att_export()
  expect_equal(length(manifest$sources), 2)
})

test_that("att_export errors on empty store", {
  store <- withr::local_tempdir()
  expect_snapshot(att_export(store = store), error = TRUE)
})

test_that("att_export excludes archived provenance records", {
  ts <- create_test_store()
  old_store <- getOption("attest.store")
  withr::defer(options(attest.store = old_store))
  att_store(ts$store)

  # Create an archive/ subdirectory with an old provenance record
  archive_dir <- file.path(
    ts$source_dir, "archive", "20260101-120000", "_attest"
  )
  dir.create(archive_dir, recursive = TRUE)
  jsonlite::write_json(
    list(name = "old-version"),
    file.path(archive_dir, "provenance.json"),
    auto_unbox = TRUE
  )

  manifest <- att_export()
  expect_equal(length(manifest$sources), 1)
  expect_equal(manifest$sources[[1]]$name, "test-source")
})
