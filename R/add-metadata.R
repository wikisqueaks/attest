#' Add Metadata Files to an Existing Source
#'
#' Downloads one or more metadata files and adds them to an already-attested
#' source. The new files are placed in the `metadata/` subdirectory and the
#' provenance record is updated. Existing files are not modified.
#'
#' @param source A source name (character) or [att_source()] object.
#' @param urls A character vector of metadata URLs to download. Named elements
#'   use the name as the local filename; unnamed elements derive the filename
#'   from the URL.
#' @param store Path to the provenance store. Defaults to [att_store()].
#' @return The updated provenance record, invisibly.
#' @export
#' @examples
#' \dontrun{
#' att_add_metadata("my-source", "https://example.com/codebook.pdf")
#'
#' att_add_metadata(
#'   "my-source",
#'   c(codebook = "https://example.com/codebook.pdf",
#'     schema   = "https://example.com/schema.xml")
#' )
#' }
att_add_metadata <- function(source, urls, store = NULL) {
  name <- resolve_source_name(source)
  if (is.null(store)) store <- att_store()

  if (!is.character(urls) || length(urls) == 0) {
    cli::cli_abort("{.arg urls} must be a non-empty character vector.")
  }

  prov_path <- provenance_path(store, name)
  if (!file.exists(prov_path)) {
    cli::cli_abort("No provenance record found for {.val {name}}.")
  }

  prov <- jsonlite::read_json(prov_path)

  source_dir <- file.path(store, name)
  metadata_dir <- file.path(source_dir, "metadata")
  dir.create(metadata_dir, recursive = TRUE, showWarnings = FALSE)

  # Determine filenames and check for duplicates with existing records
  fnames <- vapply(seq_along(urls), function(i) {
    derive_filename(urls, i)
  }, character(1))

  already_present <- fnames[fnames %in% names(prov$files)]
  if (length(already_present) > 0) {
    cli::cli_abort(c(
      "File{?s} already in provenance record: {.file {already_present}}.",
      "i" = "Use {.fun att_refresh} to update existing files."
    ))
  }

  cli::cli_alert_info(
    "Downloading {length(urls)} metadata file{?s} for {.val {name}}..."
  )

  failures <- character(0)

  for (i in seq_along(urls)) {
    url <- urls[i]
    fname <- fnames[i]
    dest <- file.path(metadata_dir, fname)

    result <- download_file(url, dest, overwrite = FALSE)
    result$location <- "metadata"
    prov$files[[fname]] <- result

    if (!is.null(result$error)) failures <- c(failures, fname)
  }

  prov$last_updated <- timestamp_now()

  jsonlite::write_json(
    prov,
    prov_path,
    pretty = TRUE, auto_unbox = TRUE
  )

  if (length(failures) > 0) {
    cli::cli_alert_warning(
      "{length(failures)} metadata file{?s} failed to download."
    )
    for (f in failures) {
      cli::cli_alert_danger("  {.file {f}}: {prov$files[[f]]$error}")
    }
  } else {
    cli::cli_alert_success(
      "Added {length(urls)} metadata file{?s} to {.val {name}}."
    )
  }

  invisible(prov)
}
