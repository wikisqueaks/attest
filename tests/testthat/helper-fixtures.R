# Helper: create a minimal provenance store with one source for testing.
# Returns a list with store path, source name, and file paths.
create_test_store <- function() {
  store <- withr::local_tempdir(.local_envir = parent.frame())
  name <- "test-source"
  source_dir <- file.path(store, name)
  attest_dir <- file.path(source_dir, "_acquire")
  metadata_dir <- file.path(source_dir, "metadata")
  dir.create(attest_dir, recursive = TRUE)
  dir.create(metadata_dir, recursive = TRUE)

  # Create a data file
  data_file <- file.path(source_dir, "data.csv")
  writeLines("a,b,c\n1,2,3", data_file)
  data_hash <- att_hash(data_file)

  # Create a metadata file
  meta_file <- file.path(metadata_dir, "codebook.txt")
  writeLines("Column a: integer\nColumn b: integer", meta_file)
  meta_hash <- att_hash(meta_file)

  # Write provenance.json
  prov <- list(
    name = name,
    dir_name = name,
    landing_url = "https://example.com/data",
    metadata = list(
      title = "Test Dataset",
      publisher = "Test Agency",
      year = "2025"
    ),
    files = list(
      "data.csv" = list(
        url = "https://example.com/data.csv",
        downloaded = "2025-01-01T12:00:00-0700",
        size = file.size(data_file),
        sha256 = data_hash,
        http_etag = NULL,
        http_last_modified = NULL,
        http_content_type = "text/csv",
        skipped = FALSE,
        error = NULL,
        location = "root"
      ),
      "codebook.txt" = list(
        url = "https://example.com/codebook.txt",
        downloaded = "2025-01-01T12:00:00-0700",
        size = file.size(meta_file),
        sha256 = meta_hash,
        http_etag = NULL,
        http_last_modified = NULL,
        http_content_type = "text/plain",
        skipped = FALSE,
        error = NULL,
        location = "metadata"
      )
    ),
    created = "2025-01-01T12:00:00-0700",
    last_updated = "2025-01-01T12:00:00-0700",
    attest_version = "0.1.0"
  )

  jsonlite::write_json(
    prov,
    file.path(attest_dir, "provenance.json"),
    pretty = TRUE, auto_unbox = TRUE
  )

  list(
    store = store,
    name = name,
    source_dir = source_dir,
    data_file = data_file,
    meta_file = meta_file,
    data_hash = data_hash,
    meta_hash = meta_hash,
    source = att_source(
      name = name,
      landing_url = "https://example.com/data",
      data_urls = c("data.csv" = "https://example.com/data.csv"),
      metadata_urls = c("codebook.txt" = "https://example.com/codebook.txt"),
      title = "Test Dataset",
      publisher = "Test Agency",
      year = "2025"
    )
  )
}
