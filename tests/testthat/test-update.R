test_that("att_update modifies metadata fields in provenance", {
  ts <- create_test_store()
  old_store <- getOption("attest.store")
  withr::defer(options(attest.store = old_store))
  att_store(ts$store)

  att_update(ts$name, title = "New Title", publisher = "New Agency")

  prov <- att_read_provenance(ts$name)
  expect_equal(prov$metadata$title, "New Title")
  expect_equal(prov$metadata$publisher, "New Agency")
})

test_that("att_update modifies landing_url", {
  ts <- create_test_store()
  old_store <- getOption("attest.store")
  withr::defer(options(attest.store = old_store))
  att_store(ts$store)

  att_update(ts$name, landing_url = "https://new-url.example.com")

  prov <- att_read_provenance(ts$name)
  expect_equal(prov$landing_url, "https://new-url.example.com")
})

test_that("att_update regenerates bib entry", {
  ts <- create_test_store()
  old_store <- getOption("attest.store")
  withr::defer(options(attest.store = old_store))
  att_store(ts$store)

  # Create initial bib
  att_cite(ts$source, store = ts$store)
  bib_path <- file.path(ts$store, "data-sources.bib")
  old_bib <- readLines(bib_path)
  expect_true(any(grepl("Test Dataset", old_bib)))

  # Update title — bib should reflect the new title
  att_update(ts$name, title = "Updated Dataset Name")

  new_bib <- readLines(bib_path)
  expect_true(any(grepl("Updated Dataset Name", new_bib)))
  expect_false(any(grepl("Test Dataset", new_bib)))
})

test_that("att_update preserves unmodified fields", {
  ts <- create_test_store()
  old_store <- getOption("attest.store")
  withr::defer(options(attest.store = old_store))
  att_store(ts$store)

  prov_before <- att_read_provenance(ts$name)

  att_update(ts$name, year = "2030")

  prov_after <- att_read_provenance(ts$name)
  expect_equal(prov_after$metadata$year, "2030")
  # Other metadata unchanged
  expect_equal(prov_after$metadata$title, prov_before$metadata$title)
  expect_equal(prov_after$metadata$publisher, prov_before$metadata$publisher)
  # File records unchanged
  expect_equal(prov_after$files, prov_before$files)
})

test_that("att_update with no arguments does nothing", {
  ts <- create_test_store()
  old_store <- getOption("attest.store")
  withr::defer(options(attest.store = old_store))
  att_store(ts$store)

  prov_before <- att_read_provenance(ts$name)
  att_update(ts$name)
  prov_after <- att_read_provenance(ts$name)

  # last_updated should not change
  expect_equal(prov_after$last_updated, prov_before$last_updated)
})

test_that("att_update errors on nonexistent source", {
  store <- withr::local_tempdir()
  old_store <- getOption("attest.store")
  withr::defer(options(attest.store = old_store))
  att_store(store)

  expect_error(att_update("nonexistent", title = "x"), "No provenance")
})
