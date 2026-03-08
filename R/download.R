#' Download Source Files
#'
#' Downloads all data and metadata files for a source, computes SHA-256 hashes,
#' records HTTP metadata, and writes a `provenance.json` record.
#'
#' Data files are saved directly in the source directory. Metadata files
#' (codebooks, data dictionaries) go in a `metadata/` subdirectory. The
#' provenance record goes in a `_acquire/` subdirectory.
#'
#' If individual file downloads fail, the provenance record is still written
#' with error information for the failed files. A warning summarizes any
#' failures.
#'
#' @param source An [acq_source()] object.
#' @param store Path to the provenance store. Defaults to [acq_store()].
#' @param overwrite Logical; if `TRUE`, re-download files that already exist
#'   locally. Default is `FALSE`.
#' @param cite Logical; if `TRUE` (default), generate a BibTeX citation and
#'   append it to `data-sources.bib` in the store root.
#' @return A list containing the full provenance record, invisibly.
#' @export
#' @examples
#' \dontrun{
#' src <- acq_source(
#'   name = "example",
#'   data_urls = c(data = "https://example.com/data.csv")
#' )
#' acq_download(src)
#' }
acq_download <- function(source, store = NULL, overwrite = FALSE,
                         cite = TRUE) {
  if (!inherits(source, "acq_source")) {
    cli::cli_abort("{.arg source} must be an {.cls acq_source} object.")
  }

  if (is.null(store)) store <- acq_store()

  source_dir <- file.path(store, source$dir_name)
  metadata_dir <- file.path(source_dir, "metadata")
  provenance_dir <- file.path(source_dir, "_acquire")

  dir.create(source_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(metadata_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(provenance_dir, recursive = TRUE, showWarnings = FALSE)

  # Preserve created timestamp from existing provenance record
  existing_prov_path <- file.path(provenance_dir, "provenance.json")
  created <- if (file.exists(existing_prov_path)) {
    existing_prov <- jsonlite::read_json(existing_prov_path)
    existing_prov$created %||% timestamp_now()
  } else {
    timestamp_now()
  }

  files_record <- list()
  failures <- character(0)

  # Download data files (to source root)
  if (length(source$data_urls) > 0) {
    cli::cli_alert_info(
      "Downloading {length(source$data_urls)} data file{?s}..."
    )
    for (i in seq_along(source$data_urls)) {
      url <- source$data_urls[i]
      fname <- derive_filename(source$data_urls, i)
      dest <- file.path(source_dir, fname)

      result <- download_file(url, dest, overwrite = overwrite)
      result$location <- "root"
      files_record[[fname]] <- result
      if (!is.null(result$error)) failures <- c(failures, fname)
    }
  }

  # Download metadata files (to metadata/ subdirectory)
  if (length(source$metadata_urls) > 0) {
    cli::cli_alert_info(
      "Downloading {length(source$metadata_urls)} metadata file{?s}..."
    )
    for (i in seq_along(source$metadata_urls)) {
      url <- source$metadata_urls[i]
      fname <- derive_filename(source$metadata_urls, i)
      dest <- file.path(metadata_dir, fname)

      result <- download_file(url, dest, overwrite = overwrite)
      result$location <- "metadata"
      files_record[[fname]] <- result
      if (!is.null(result$error)) failures <- c(failures, fname)
    }
  }

  # Write provenance record (even on partial failure)
  provenance <- list(
    name = source$name,
    dir_name = source$dir_name,
    landing_url = source$landing_url,
    metadata = source$metadata,
    files = files_record,
    created = created,
    last_updated = timestamp_now(),
    acquire_version = as.character(utils::packageVersion("acquire"))
  )

  jsonlite::write_json(
    provenance,
    file.path(provenance_dir, "provenance.json"),
    pretty = TRUE, auto_unbox = TRUE
  )

  if (cite && length(failures) == 0) {
    acq_cite(source, store = store)
  }

  if (length(failures) > 0) {
    cli::cli_alert_warning(
      "Source {.val {source$name}}: {length(failures)} file{?s} failed"
    )
    for (f in failures) {
      cli::cli_alert_danger("  {.file {f}}: {files_record[[f]]$error}")
    }
  } else {
    cli::cli_alert_success(
      "Source {.val {source$name}} downloaded to {.path {source_dir}}"
    )
  }
  invisible(provenance)
}
