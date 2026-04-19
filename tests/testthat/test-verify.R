test_that("att_verify passes for unmodified files", {
  fix <- create_test_store()
  result <- att_verify(fix$source, store = fix$store)

  expect_true(result$all_ok)
  expect_equal(result$files[["data.csv"]]$status, "ok")
  expect_equal(result$files[["codebook.txt"]]$status, "ok")
})

test_that("att_verify detects modified data file", {
  fix <- create_test_store()
  writeLines("modified content", fix$data_file)

  result <- att_verify(fix$source, store = fix$store)

  expect_false(result$all_ok)
  expect_equal(result$files[["data.csv"]]$status, "changed")
  expect_equal(result$files[["codebook.txt"]]$status, "ok")
})

test_that("att_verify detects modified metadata file", {
  fix <- create_test_store()
  writeLines("changed codebook", fix$meta_file)

  result <- att_verify(fix$source, store = fix$store)

  expect_false(result$all_ok)
  expect_equal(result$files[["data.csv"]]$status, "ok")
  expect_equal(result$files[["codebook.txt"]]$status, "changed")
})

test_that("att_verify detects missing file", {
  fix <- create_test_store()
  unlink(fix$data_file)

  result <- att_verify(fix$source, store = fix$store)

  expect_false(result$all_ok)
  expect_equal(result$files[["data.csv"]]$status, "missing")
})

test_that("att_verify errors for unknown source", {
  store <- withr::local_tempdir()
  expect_error(att_verify("nonexistent", store = store))
})

test_that("att_verify accepts source name as string", {
  fix <- create_test_store()
  result <- att_verify("test-source", store = fix$store)
  expect_true(result$all_ok)
})

test_that("att_verify skips directory-format entries with no sha256", {
  store <- withr::local_tempdir()
  name <- "gdb-source"
  source_dir <- file.path(store, name)
  attest_dir <- file.path(source_dir, "_attest")
  dir.create(attest_dir, recursive = TRUE)

  # Fake .gdb directory in the store
  gdb_dir <- file.path(source_dir, "mydata.gdb")
  dir.create(gdb_dir)
  writeLines("internal", file.path(gdb_dir, "file.gdbtable"))

  # A regular tracked file alongside it
  data_file <- file.path(source_dir, "data.csv")
  writeLines("a,b\n1,2", data_file)

  prov <- list(
    name = name, dir_name = name, origin = "remote",
    metadata = list(title = "GDB Test"),
    files = list(
      "mydata.gdb" = list(
        extracted_from = "data.zip", type = "directory",
        location = "root", error = NULL
      ),
      "data.csv" = list(
        extracted_from = "data.zip", size = file.size(data_file),
        sha256 = att_hash(data_file), location = "root", error = NULL
      )
    )
  )
  jsonlite::write_json(prov, file.path(attest_dir, "provenance.json"),
                       pretty = TRUE, auto_unbox = TRUE)

  result <- att_verify(name, store = store)

  expect_true(result$all_ok)
  expect_equal(result$files[["mydata.gdb"]]$status, "skipped")
  expect_equal(result$files[["data.csv"]]$status, "ok")
})
