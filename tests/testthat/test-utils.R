test_that("derive_filename uses name when provided", {
  urls <- c(myfile.csv = "https://example.com/data.csv")
  expect_equal(attest:::derive_filename(urls, 1), "myfile.csv")
})

test_that("derive_filename falls back to URL basename", {
  urls <- c("https://example.com/path/to/data.csv")
  expect_equal(attest:::derive_filename(urls, 1), "data.csv")
})

test_that("derive_filename strips query strings", {
  urls <- c("https://example.com/data.csv?format=raw&key=abc")
  expect_equal(attest:::derive_filename(urls, 1), "data.csv")
})

test_that("derive_filename strips fragment identifiers", {
  urls <- c("https://example.com/data.csv#section")
  expect_equal(attest:::derive_filename(urls, 1), "data.csv")
})

test_that("resolve_source_name works with strings", {
  expect_equal(attest:::resolve_source_name("my-source"), "my-source")
})

test_that("resolve_source_name works with att_source objects", {
  src <- att_source(name = "test source")
  expect_equal(attest:::resolve_source_name(src), "test-source")
})

test_that("resolve_source_name rejects invalid input", {
  expect_error(attest:::resolve_source_name(123))
  expect_error(attest:::resolve_source_name(c("a", "b")))
})

test_that("att_request sets user-agent and timeout", {
  req <- attest:::att_request("https://example.com")
  expect_s3_class(req, "httr2_request")
})
