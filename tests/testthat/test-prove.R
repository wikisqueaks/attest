test_that("att_prove generates a report for a single source", {
  ts <- create_test_store()
  old_store <- getOption("attest.store")
  withr::defer(options(attest.store = old_store))
  att_store(ts$store)

  report <- att_prove()

  expect_s3_class(report, "att_report")
  expect_equal(report$report_type, "attest_provenance_report")
  expect_equal(report$summary$total_sources, 1)
  expect_true(report$summary$total_files >= 1)
  expect_true(report$summary$all_verified)
  expect_equal(report$summary$files_failed, 0)

  # Source entry
  src <- report$sources[["test-source"]]
  expect_equal(src$name, "test-source")
  expect_true(src$verified)
  expect_true(length(src$files) >= 1)
  # All files should be ok
  statuses <- vapply(src$files, `[[`, character(1), "status")
  expect_true(all(statuses == "ok"))
})

test_that("att_prove writes a JSON report file", {
  ts <- create_test_store()
  old_store <- getOption("attest.store")
  withr::defer(options(attest.store = old_store))
  att_store(ts$store)

  report <- att_prove()
  report_path <- file.path(ts$store, "provenance-report.json")

  expect_true(file.exists(report_path))

  # Verify it's valid JSON
  parsed <- jsonlite::read_json(report_path)
  expect_equal(parsed$report_type, "attest_provenance_report")
})

test_that("att_prove writes to custom output path", {
  ts <- create_test_store()
  old_store <- getOption("attest.store")
  withr::defer(options(attest.store = old_store))
  att_store(ts$store)

  custom_path <- file.path(ts$store, "reports", "custom-report.json")
  att_prove(output = custom_path)

  expect_true(file.exists(custom_path))
})

test_that("att_prove detects modified files", {
  ts <- create_test_store()
  old_store <- getOption("attest.store")
  withr::defer(options(attest.store = old_store))
  att_store(ts$store)

  # Tamper with the data file
  writeLines("tampered content", ts$data_file)

  report <- att_prove()

  expect_false(report$summary$all_verified)
  expect_equal(report$summary$files_failed, 1)

  src <- report$sources[["test-source"]]
  expect_false(src$verified)
  expect_equal(src$files[[1]]$status, "changed")
})

test_that("att_prove handles multiple sources", {
  store <- withr::local_tempdir()
  old_store <- getOption("attest.store")
  withr::defer(options(attest.store = old_store))
  att_store(store)

  # Create two sources
  for (sname in c("source-a", "source-b")) {
    source_dir <- file.path(store, sname)
    attest_dir <- file.path(source_dir, "_attest")
    dir.create(attest_dir, recursive = TRUE)

    data_file <- file.path(source_dir, "data.csv")
    writeLines("x,y\n1,2", data_file)

    prov <- list(
      name = sname,
      dir_name = sname,
      origin = "local",
      files = list(
        "data.csv" = list(
          sha256 = att_hash(data_file),
          size = file.size(data_file),
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

  report <- att_prove()

  expect_equal(report$summary$total_sources, 2)
  expect_equal(report$summary$total_files, 2)
  expect_true(report$summary$all_verified)
})

test_that("att_prove errors on empty store", {
  store <- withr::local_tempdir()
  expect_error(att_prove(store = store), "No sources found")
})
