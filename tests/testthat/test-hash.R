test_that("acq_hash computes a SHA-256 hash", {
  tf <- withr::local_tempfile()
  writeLines("hello world", tf)

  hash <- acq_hash(tf)
  expect_type(hash, "character")
  expect_equal(nchar(hash), 64) # SHA-256 produces 64 hex characters
})

test_that("acq_hash is reproducible", {
  tf <- withr::local_tempfile()
  writeLines("test content", tf)

  expect_equal(acq_hash(tf), acq_hash(tf))
})

test_that("acq_hash detects content changes", {
  tf <- withr::local_tempfile()

  writeLines("version 1", tf)
  hash1 <- acq_hash(tf)

  writeLines("version 2", tf)
  hash2 <- acq_hash(tf)

  expect_false(hash1 == hash2)
})

test_that("acq_hash errors on missing file", {
  expect_error(acq_hash("nonexistent-file.txt"), "File not found")
})
