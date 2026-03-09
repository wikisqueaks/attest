test_that("build_bib_entry creates valid BibTeX", {
  entry <- build_bib_entry("mykey", "misc", list(
    title = "My Title",
    author = "{My Author}",
    year = "2025"
  ))
  expect_match(entry, "@misc\\{mykey,")
  expect_match(entry, "title = \\{My Title\\}")
  expect_match(entry, "year = \\{2025\\}")
})

test_that("build_bib_entry drops NULL fields", {
  entry <- build_bib_entry("mykey", "misc", list(
    title = "My Title",
    author = NULL,
    year = "2025"
  ))
  expect_no_match(entry, "author")
})

test_that("att_cite generates correct BibTeX entry", {
  fix <- create_test_store()
  result <- att_cite(fix$source, store = fix$store, write = FALSE)

  expect_match(result, "@misc\\{test_source,")
  expect_match(result, "title = \\{Test Dataset\\}")
  expect_match(result, "author = \\{\\{Test Agency\\}\\}")
  expect_match(result, "year = \\{2025\\}")
  expect_match(result, "url = \\{https://example.com/data\\}")
  expect_match(result, "urldate")
})

test_that("att_cite writes to data-sources.bib", {
  fix <- create_test_store()
  att_cite(fix$source, store = fix$store)

  bib_path <- file.path(fix$store, "data-sources.bib")
  expect_true(file.exists(bib_path))

  content <- readLines(bib_path)
  expect_true(any(grepl("test_source", content)))
  expect_true(any(grepl("Test Dataset", content)))
})

test_that("att_cite replaces existing entry on re-cite", {
  fix <- create_test_store()
  att_cite(fix$source, store = fix$store)
  att_cite(fix$source, store = fix$store)

  bib_path <- file.path(fix$store, "data-sources.bib")
  content <- paste(readLines(bib_path), collapse = "\n")

  # Should appear exactly once
  matches <- gregexpr("test_source", content)[[1]]
  expect_equal(length(matches), 1)
})

test_that("att_cite does not include SHA hashes", {
  fix <- create_test_store()
  result <- att_cite(fix$source, store = fix$store, write = FALSE)

  expect_no_match(result, "SHA-256")
  expect_no_match(result, fix$data_hash)
})

test_that("att_cite uses custom key when provided", {
  fix <- create_test_store()
  result <- att_cite(fix$source, store = fix$store, key = "my_custom_key",
                     write = FALSE)
  expect_match(result, "@misc\\{my_custom_key,")
})
