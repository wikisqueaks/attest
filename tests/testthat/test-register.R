test_that("acq_register rejects non-acq_source input", {
  expect_error(acq_register("not a source"), "acq_source")
})

test_that("acq_register rejects sources with URLs", {
  src <- acq_source(
    name = "remote-src",
    data_urls = c("https://example.com/data.csv"),
    title = "Remote Source"
  )
  expect_error(acq_register(src), "acq_download")
})

test_that("acq_register errors when no paths are provided", {
  src <- acq_source(name = "empty", title = "Empty Source")
  expect_error(acq_register(src), "data_paths")
})

test_that("acq_register copies files into the store", {
  store <- withr::local_tempdir()
  old_store <- getOption("acquire.store")
  withr::defer(options(acquire.store = old_store))
  acq_store(store)

  # Create source files outside the store
  ext_dir <- withr::local_tempdir()
  data_file <- file.path(ext_dir, "raw-data.csv")
  meta_file <- file.path(ext_dir, "codebook.txt")
  writeLines("x,y\n1,2\n3,4", data_file)
  writeLines("x: numeric\ny: numeric", meta_file)

  original_data_hash <- acq_hash(data_file)
  original_meta_hash <- acq_hash(meta_file)

  src <- acq_source(
    name = "copy-test",
    data_paths = c("data.csv" = data_file),
    metadata_paths = c("codebook.txt" = meta_file),
    title = "Copy Test"
  )

  prov <- acq_register(src, cite = FALSE)

  # Files copied to correct locations
  expect_true(file.exists(file.path(store, "copy-test", "data.csv")))
  expect_true(file.exists(file.path(store, "copy-test", "metadata", "codebook.txt")))

  # Originals still exist

  expect_true(file.exists(data_file))
  expect_true(file.exists(meta_file))

  # Hashes match originals
  expect_equal(prov$files[["data.csv"]]$sha256, original_data_hash)
  expect_equal(prov$files[["codebook.txt"]]$sha256, original_meta_hash)

  # Provenance record has correct structure
  expect_equal(prov$origin, "local")
  expect_equal(prov$files[["data.csv"]]$location, "root")
  expect_equal(prov$files[["codebook.txt"]]$location, "metadata")
  expect_false(is.null(prov$files[["data.csv"]]$source_path))
  expect_null(prov$files[["data.csv"]]$error)
})

test_that("acq_register with move = TRUE deletes originals", {
  store <- withr::local_tempdir()
  old_store <- getOption("acquire.store")
  withr::defer(options(acquire.store = old_store))
  acq_store(store)

  ext_dir <- withr::local_tempdir()
  data_file <- file.path(ext_dir, "to-move.csv")
  writeLines("a,b\n10,20", data_file)
  original_hash <- acq_hash(data_file)

  src <- acq_source(
    name = "move-test",
    data_paths = c("data.csv" = data_file),
    title = "Move Test"
  )

  prov <- acq_register(src, move = TRUE, cite = FALSE)

  # File exists in store
  expect_true(file.exists(file.path(store, "move-test", "data.csv")))

  # Original deleted
  expect_false(file.exists(data_file))

  # Hash matches
  expect_equal(prov$files[["data.csv"]]$sha256, original_hash)
})

test_that("acq_register handles files already in place", {
  store <- withr::local_tempdir()
  old_store <- getOption("acquire.store")
  withr::defer(options(acquire.store = old_store))
  acq_store(store)

  # Pre-create the store directory structure and place the file
  source_dir <- file.path(store, "inplace-test")
  dir.create(source_dir, recursive = TRUE)
  data_file <- file.path(source_dir, "data.csv")
  writeLines("p,q\n5,6", data_file)
  original_hash <- acq_hash(data_file)

  src <- acq_source(
    name = "inplace-test",
    data_paths = c("data.csv" = data_file),
    title = "In-Place Test"
  )

  prov <- acq_register(src, cite = FALSE)

  # File still there, not duplicated
  expect_true(file.exists(data_file))
  expect_equal(prov$files[["data.csv"]]$sha256, original_hash)
  expect_null(prov$files[["data.csv"]]$error)
})

test_that("acq_register handles files already in place with move = TRUE", {
  store <- withr::local_tempdir()
  old_store <- getOption("acquire.store")
  withr::defer(options(acquire.store = old_store))
  acq_store(store)

  # File already in the correct location — move should not delete it
  source_dir <- file.path(store, "inplace-move")
  dir.create(source_dir, recursive = TRUE)
  data_file <- file.path(source_dir, "data.csv")
  writeLines("r,s\n7,8", data_file)

  src <- acq_source(
    name = "inplace-move",
    data_paths = c("data.csv" = data_file),
    title = "In-Place Move Test"
  )

  prov <- acq_register(src, move = TRUE, cite = FALSE)

  # File should NOT be deleted when it's already in place
  expect_true(file.exists(data_file))
  expect_null(prov$files[["data.csv"]]$error)
})

test_that("acq_register writes valid provenance.json", {
  store <- withr::local_tempdir()
  old_store <- getOption("acquire.store")
  withr::defer(options(acquire.store = old_store))
  acq_store(store)

  ext_dir <- withr::local_tempdir()
  data_file <- file.path(ext_dir, "prov-data.csv")
  writeLines("col1\nval1", data_file)

  src <- acq_source(
    name = "prov-test",
    data_paths = c("data.csv" = data_file),
    title = "Provenance Test",
    publisher = "Test Org",
    year = "2026"
  )

  acq_register(src, cite = FALSE)

  prov <- acq_read_provenance(src, store = store)
  expect_equal(prov$name, "prov-test")
  expect_equal(prov$origin, "local")
  expect_equal(length(prov$files), 1)
  expect_equal(nchar(prov$files[["data.csv"]]$sha256), 64)
  expect_false(is.null(prov$created))
  expect_false(is.null(prov$last_updated))
})

test_that("acq_register refuses to re-register existing source", {
  store <- withr::local_tempdir()
  old_store <- getOption("acquire.store")
  withr::defer(options(acquire.store = old_store))
  acq_store(store)

  ext_dir <- withr::local_tempdir()
  data_file <- file.path(ext_dir, "dup.csv")
  writeLines("z\n1", data_file)

  src <- acq_source(
    name = "dup-test",
    data_paths = c("data.csv" = data_file),
    title = "Duplicate Test"
  )

  acq_register(src, cite = FALSE)
  expect_error(acq_register(src, cite = FALSE), "already been registered")
})

test_that("acq_register records errors for missing source files", {
  store <- withr::local_tempdir()
  old_store <- getOption("acquire.store")
  withr::defer(options(acquire.store = old_store))
  acq_store(store)

  src <- acq_source(
    name = "missing-test",
    data_paths = c("data.csv" = "/nonexistent/path/data.csv"),
    title = "Missing File Test"
  )

  prov <- acq_register(src, cite = FALSE)
  expect_false(is.null(prov$files[["data.csv"]]$error))
  expect_true(grepl("not found", prov$files[["data.csv"]]$error, ignore.case = TRUE))
})

test_that("acq_register with move = TRUE keeps originals on partial failure", {
  store <- withr::local_tempdir()
  old_store <- getOption("acquire.store")
  withr::defer(options(acquire.store = old_store))
  acq_store(store)

  ext_dir <- withr::local_tempdir()
  good_file <- file.path(ext_dir, "good.csv")
  writeLines("x\n1", good_file)

  src <- acq_source(
    name = "partial-move",
    data_paths = c(
      "good.csv" = good_file,
      "bad.csv" = "/nonexistent/path/bad.csv"
    ),
    title = "Partial Move Test"
  )

  prov <- acq_register(src, move = TRUE, cite = FALSE)

  # Good file should still exist (not deleted due to partial failure)
  expect_true(file.exists(good_file))
  # But it was copied into the store
  expect_true(file.exists(file.path(store, "partial-move", "good.csv")))
  # Bad file recorded the error
  expect_false(is.null(prov$files[["bad.csv"]]$error))
})

test_that("acq_register uses basename when paths are unnamed", {
  store <- withr::local_tempdir()
  old_store <- getOption("acquire.store")
  withr::defer(options(acquire.store = old_store))
  acq_store(store)

  ext_dir <- withr::local_tempdir()
  data_file <- file.path(ext_dir, "my-data.csv")
  writeLines("w\n9", data_file)

  src <- acq_source(
    name = "basename-test",
    data_paths = data_file,
    title = "Basename Test"
  )

  prov <- acq_register(src, cite = FALSE)

  # Should use the basename "my-data.csv" as the filename
  expect_true("my-data.csv" %in% names(prov$files))
  expect_true(file.exists(file.path(store, "basename-test", "my-data.csv")))
})
