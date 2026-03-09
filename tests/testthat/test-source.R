test_that("att_source creates a valid source object", {
  src <- att_source(
    name = "test-source",
    landing_url = "https://example.com",
    data_urls = c(main = "https://example.com/data.csv"),
    metadata_urls = c(codebook = "https://example.com/codebook.pdf"),
    title = "Test Data",
    publisher = "Test Agency",
    year = "2025"
  )

  expect_s3_class(src, "att_source")
  expect_equal(src$name, "test-source")
  expect_equal(src$dir_name, "test-source")
  expect_equal(src$landing_url, "https://example.com")
  expect_equal(src$data_urls, c(main = "https://example.com/data.csv"))
  expect_equal(src$metadata_urls, c(codebook = "https://example.com/codebook.pdf"))
  expect_equal(src$metadata$title, "Test Data")
  expect_equal(src$metadata$publisher, "Test Agency")
  expect_equal(src$metadata$year, "2025")
})

test_that("att_source sanitizes names for directory use", {
  src <- att_source(name = "My Source (2025)")
  expect_equal(src$dir_name, "My-Source--2025-")
})

test_that("att_source rejects invalid names", {
  expect_error(att_source(name = ""), "non-empty")
  expect_error(att_source(name = c("a", "b")), "single character")
  expect_error(att_source(name = 123), "single character")
})

test_that("att_source merges explicit and list metadata", {
  src <- att_source(
    name = "test",
    title = "My Title",
    metadata = list(license = "CC-BY-4.0")
  )

  expect_equal(src$metadata$title, "My Title")
  expect_equal(src$metadata$license, "CC-BY-4.0")
})

test_that("print.att_source runs without error", {
  src <- att_source(
    name = "test",
    landing_url = "https://example.com",
    data_urls = c("https://example.com/a.csv", "https://example.com/b.csv"),
    title = "Test"
  )

  expect_no_error(print(src))
  expect_invisible(print(src))
})

test_that("att_source rejects mixing URLs and paths", {
  expect_error(
    att_source(
      name = "mixed",
      data_urls = c("https://example.com/data.csv"),
      data_paths = c("~/local/data.csv")
    ),
    "cannot mix"
  )

  expect_error(
    att_source(
      name = "mixed",
      data_urls = c("https://example.com/data.csv"),
      metadata_paths = c("~/local/codebook.pdf")
    ),
    "cannot mix"
  )
})

test_that("att_source accepts local paths without URLs", {
  src <- att_source(
    name = "local-only",
    data_paths = c(data = "~/data/file.csv"),
    title = "Local Source"
  )

  expect_s3_class(src, "att_source")
  expect_equal(src$data_paths, c(data = "~/data/file.csv"))
  expect_null(src$data_urls)
})
