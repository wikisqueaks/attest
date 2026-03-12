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

# -- Markdown citation generation ----------------------------------------------

test_that("att_cite generates data-sources.md alongside bib", {
  fix <- create_test_store()
  att_cite(fix$source, store = fix$store)

  md_path <- file.path(fix$store, "data-sources.md")
  expect_true(file.exists(md_path))

  content <- readLines(md_path)
  expect_equal(content[1], "# Data Sources")
  expect_true(any(grepl("Test Agency", content)))
  expect_true(any(grepl("\\*Test Dataset\\*", content)))
  expect_true(any(grepl("2025", content)))
})

test_that("parse_bib_file extracts fields correctly", {
  tmp <- withr::local_tempdir()
  bib_path <- file.path(tmp, "test.bib")
  entry <- build_bib_entry("my_key", "misc", list(
    title = "My Title",
    author = "{My Author}",
    year = "2025",
    url = "https://example.com"
  ))
  writeLines(entry, bib_path)

  parsed <- parse_bib_file(bib_path)
  expect_length(parsed, 1)
  expect_equal(parsed[[1]]$key, "my_key")
  expect_equal(parsed[[1]]$title, "My Title")
  expect_equal(parsed[[1]]$author, "My Author")
  expect_equal(parsed[[1]]$year, "2025")
  expect_equal(parsed[[1]]$url, "https://example.com")
})

test_that("parse_bib_file handles multiple entries", {
  tmp <- withr::local_tempdir()
  bib_path <- file.path(tmp, "test.bib")
  e1 <- build_bib_entry("a", "misc", list(title = "First", year = "2024"))
  e2 <- build_bib_entry("b", "misc", list(title = "Second", year = "2025"))
  writeLines(paste(e1, e2, sep = "\n\n"), bib_path)

  parsed <- parse_bib_file(bib_path)
  expect_length(parsed, 2)
})

test_that("format_markdown_citation produces APA-style output", {
  entry <- list(
    author = "Statistics Canada",
    year = "2024",
    title = "Census Data",
    note = "[Data set; CSV]",
    url = "https://example.com"
  )
  result <- format_markdown_citation(entry)
  expect_equal(
    result,
    "Statistics Canada. (2024). *Census Data* [Data set; CSV]. https://example.com"
  )
})

test_that("sync_markdown_citations sorts by author", {
  tmp <- withr::local_tempdir()
  bib_path <- file.path(tmp, "data-sources.bib")

  e1 <- build_bib_entry("z_source", "misc", list(
    title = "Zebra Data", author = "{Zed Agency}", year = "2025"
  ))
  e2 <- build_bib_entry("a_source", "misc", list(
    title = "Alpha Data", author = "{Alpha Bureau}", year = "2024"
  ))
  writeLines(paste(e1, e2, sep = "\n\n"), bib_path)
  sync_markdown_citations(bib_path)

  md <- readLines(file.path(tmp, "data-sources.md"))
  # Alpha should come before Zed
  alpha_line <- which(grepl("Alpha", md))
  zed_line <- which(grepl("Zed", md))
  expect_true(alpha_line < zed_line)
})

test_that("sync_markdown_citations removes md when bib is empty", {
  tmp <- withr::local_tempdir()
  bib_path <- file.path(tmp, "data-sources.bib")
  md_path <- file.path(tmp, "data-sources.md")

  # Create then empty the bib
  writeLines("", bib_path)
  writeLines("old content", md_path)
  sync_markdown_citations(bib_path)

  expect_false(file.exists(md_path))
})
