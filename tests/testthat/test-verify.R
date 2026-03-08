test_that("acq_verify passes for unmodified files", {
  fix <- create_test_store()
  result <- acq_verify(fix$source, store = fix$store)

  expect_true(result$all_ok)
  expect_equal(result$files[["data.csv"]]$status, "ok")
  expect_equal(result$files[["codebook.txt"]]$status, "ok")
})

test_that("acq_verify detects modified data file", {
  fix <- create_test_store()
  writeLines("modified content", fix$data_file)

  result <- acq_verify(fix$source, store = fix$store)

  expect_false(result$all_ok)
  expect_equal(result$files[["data.csv"]]$status, "changed")
  expect_equal(result$files[["codebook.txt"]]$status, "ok")
})

test_that("acq_verify detects modified metadata file", {
  fix <- create_test_store()
  writeLines("changed codebook", fix$meta_file)

  result <- acq_verify(fix$source, store = fix$store)

  expect_false(result$all_ok)
  expect_equal(result$files[["data.csv"]]$status, "ok")
  expect_equal(result$files[["codebook.txt"]]$status, "changed")
})

test_that("acq_verify detects missing file", {
  fix <- create_test_store()
  unlink(fix$data_file)

  result <- acq_verify(fix$source, store = fix$store)

  expect_false(result$all_ok)
  expect_equal(result$files[["data.csv"]]$status, "missing")
})

test_that("acq_verify errors for unknown source", {
  store <- withr::local_tempdir()
  expect_error(acq_verify("nonexistent", store = store))
})

test_that("acq_verify accepts source name as string", {
  fix <- create_test_store()
  result <- acq_verify("test-source", store = fix$store)
  expect_true(result$all_ok)
})
