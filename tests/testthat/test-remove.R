test_that("att_remove deletes source directory", {
  ts <- create_test_store()
  old_store <- getOption("attest.store")
  withr::defer(options(attest.store = old_store))
  att_store(ts$store)

  expect_true(dir.exists(ts$source_dir))
  result <- att_remove(ts$name, force = TRUE)
  expect_true(result)
  expect_false(dir.exists(ts$source_dir))
})

test_that("att_remove removes bib entry", {
  ts <- create_test_store()
  old_store <- getOption("attest.store")
  withr::defer(options(attest.store = old_store))
  att_store(ts$store)

  # Create a bib file with the source's entry
  att_cite(ts$source, store = ts$store)
  bib_path <- file.path(ts$store, "data-sources.bib")
  expect_true(file.exists(bib_path))
  expect_true(any(grepl("test_source", readLines(bib_path))))

  att_remove(ts$name, force = TRUE)

  # Bib file should be removed (was the only entry)
  expect_false(file.exists(bib_path))
})

test_that("att_remove preserves other bib entries", {
  ts <- create_test_store()
  old_store <- getOption("attest.store")
  withr::defer(options(attest.store = old_store))
  att_store(ts$store)

  # Create bib with the test source
  att_cite(ts$source, store = ts$store)

  # Add another entry manually
  bib_path <- file.path(ts$store, "data-sources.bib")
  cat("\n\n@misc{other_source,\n  title = {Other Data}\n}",
      file = bib_path, append = TRUE)

  att_remove(ts$name, force = TRUE)

  # Bib file should still exist with the other entry
  expect_true(file.exists(bib_path))
  content <- readLines(bib_path)
  expect_false(any(grepl("test_source", content)))
  expect_true(any(grepl("other_source", content)))
})

test_that("att_remove errors on nonexistent source", {
  store <- withr::local_tempdir()
  old_store <- getOption("attest.store")
  withr::defer(options(attest.store = old_store))
  att_store(store)

  expect_error(att_remove("nonexistent", force = TRUE), "not found")
})

test_that("att_remove errors in non-interactive without force", {
  ts <- create_test_store()
  old_store <- getOption("attest.store")
  withr::defer(options(attest.store = old_store))
  att_store(ts$store)

  withr::local_options(rlang_interactive = FALSE)
  expect_error(att_remove(ts$name), "force = TRUE")
})

test_that("format_size produces readable output", {
  expect_equal(format_size(0), "0 B")
  expect_equal(format_size(500), "500 B")
  expect_equal(format_size(1024), "1 KB")
  expect_equal(format_size(1536), "1.5 KB")
  expect_equal(format_size(1048576), "1 MB")
})
