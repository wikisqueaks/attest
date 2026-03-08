#' Archive a Source's Current Files
#'
#' Copies the current downloaded files and provenance record into a
#' timestamped subdirectory under `archive/`. Useful before refreshing a
#' source.
#'
#' @param source A source name (character) or [acq_source()] object.
#' @param store Path to the provenance store. Defaults to [acq_store()].
#' @return The archive directory path, invisibly.
#' @keywords internal
acq_archive <- function(source, store = NULL) {
  name <- resolve_source_name(source)
  if (is.null(store)) store <- acq_store()

  source_dir <- file.path(store, name)
  if (!dir.exists(source_dir)) {
    cli::cli_abort("Source directory not found: {.path {source_dir}}")
  }

  timestamp <- format(Sys.time(), "%Y%m%d-%H%M%S")
  archive_dir <- file.path(source_dir, "archive", timestamp)
  dir.create(archive_dir, recursive = TRUE, showWarnings = FALSE)

  # Copy all files (excluding the archive directory itself)
  files <- list.files(source_dir, full.names = TRUE)
  files <- files[basename(files) != "archive"]
  file.copy(files, archive_dir)

  cli::cli_alert_success("Archived {.val {name}} to {.path {archive_dir}}")
  invisible(archive_dir)
}

#' Refresh a Source
#'
#' Re-downloads all files for a previously acquired source. Optionally archives
#' the current files before overwriting them.
#'
#' @param source A source name (character) or [acq_source()] object.
#' @param store Path to the provenance store. Defaults to [acq_store()].
#' @param archive Logical; if `TRUE` (default), archive current files before
#'   re-downloading.
#' @return The updated provenance record, invisibly.
#' @keywords internal
acq_refresh <- function(source, store = NULL, archive = TRUE) {
  name <- resolve_source_name(source)
  if (is.null(store)) store <- acq_store()

  prov_path <- provenance_path(store, name)
  if (!file.exists(prov_path)) {
    cli::cli_abort("No provenance record found for {.val {name}}.")
  }

  prov <- jsonlite::read_json(prov_path)

  if (archive) {
    acq_archive(name, store)
  }

  # Reconstruct source object from provenance record
  data_urls <- character(0)
  metadata_urls <- character(0)

  for (fname in names(prov$files)) {
    file_info <- prov$files[[fname]]
    location <- file_info$location %||% "root"
    if (location == "metadata") {
      metadata_urls[fname] <- file_info$url
    } else {
      data_urls[fname] <- file_info$url
    }
  }

  src <- acq_source(
    name = prov$name,
    landing_url = prov$landing_url,
    data_urls = if (length(data_urls) > 0) data_urls else NULL,
    metadata_urls = if (length(metadata_urls) > 0) metadata_urls else NULL,
    metadata = prov$metadata
  )

  acq_download(src, store = store, overwrite = TRUE)
}
