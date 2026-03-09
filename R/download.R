#' Download Source Files
#'
#' Downloads all data and metadata files for a source, computes SHA-256 hashes,
#' records HTTP metadata, and writes a `provenance.json` record.
#'
#' Data files are saved directly in the source directory. Metadata files
#' (codebooks, data dictionaries) go in a `metadata/` subdirectory. The
#' provenance record goes in a `_attest/` subdirectory.
#'
#' If individual file downloads fail, the provenance record is still written
#' with error information for the failed files. A warning summarizes any
#' failures.
#'
#' `att_download()` is intended for first-time acquisition. If a provenance
#' record already exists for the source, it will error and recommend
#' [att_refresh()] instead.
#'
#' @section Archive (zip) support:
#'
#' When a URL in `data_urls` points to a `.zip` file, `att_download()`
#' automatically downloads and extracts the archive. In an interactive session,
#' you will be prompted to classify each extracted file as data, metadata, or
#' ignore. In non-interactive sessions, all files default to data unless
#' `classify` is provided.
#'
#' The archive itself is not retained, but its SHA-256 hash is recorded in
#' provenance for future comparison. Each extracted file records which archive
#' it came from.
#'
#' @param source An [att_source()] object.
#' @param store Path to the provenance store. Defaults to [att_store()].
#' @param cite Logical; if `TRUE` (default), generate a BibTeX citation and
#'   append it to `data-sources.bib` in the store root.
#' @param classify Optional named list for non-interactive archive file
#'   classification. Elements `data`, `metadata`, and `ignore` each contain
#'   character vectors of file extensions (e.g.,
#'   `list(data = c(".shp", ".dbf"), metadata = ".xml")`). Only used for
#'   archive URLs; ignored for regular downloads.
#' @return A list containing the full provenance record, invisibly.
#' @export
#' @examples
#' \dontrun{
#' src <- att_source(
#'   name = "example",
#'   data_urls = c(data = "https://example.com/data.csv")
#' )
#' att_download(src)
#'
#' # Archive (zip) download with interactive classification
#' shp <- att_source(
#'   name = "boundaries",
#'   landing_url = "https://example.com/geodata",
#'   data_urls = "https://example.com/boundaries.zip"
#' )
#' att_download(shp)
#'
#' # Non-interactive archive classification
#' att_download(shp, classify = list(
#'   metadata = c(".xml", ".pdf"),
#'   ignore = ".html"
#' ))
#' }
att_download <- function(source, store = NULL, cite = TRUE, classify = NULL) {
  if (!inherits(source, "att_source")) {
    cli::cli_abort("{.arg source} must be an {.cls att_source} object.")
  }

  if (!is.null(source$data_paths) || !is.null(source$metadata_paths)) {
    cli::cli_abort(c(
      "{.fun att_download} is for remote sources with URLs.",
      "i" = "This source has local paths. Use {.fun att_register} instead."
    ))
  }

  if (is.null(store)) store <- att_store()

  source_dir <- file.path(store, source$dir_name)
  provenance_dir <- file.path(source_dir, "_attest")
  existing_prov <- file.path(provenance_dir, "provenance.json")

  if (file.exists(existing_prov)) {
    cli::cli_abort(c(
      "Source {.val {source$name}} has already been downloaded.",
      "i" = "Use {.fun att_refresh} to check for updates and re-download."
    ))
  }

  metadata_dir <- file.path(source_dir, "metadata")

  dir.create(source_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(metadata_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(provenance_dir, recursive = TRUE, showWarnings = FALSE)

  created <- timestamp_now()

  files_record <- list()
  archives_record <- list()
  failures <- character(0)

  # Download data files (to source root)
  if (length(source$data_urls) > 0) {
    for (i in seq_along(source$data_urls)) {
      url <- source$data_urls[i]
      fname <- derive_filename(source$data_urls, i)

      if (is_archive_url(url)) {
        # Archive workflow: download, extract, classify, place
        archive_result <- process_archive_url(
          url, fname, source_dir, metadata_dir, classify = classify
        )
        archives_record[[fname]] <- archive_result$archive
        files_record <- c(files_record, archive_result$files)
        failures <- c(failures, archive_result$failures)
      } else {
        # Standard download
        cli::cli_alert_info(
          "Downloading {length(source$data_urls)} data file{?s}..."
        )
        dest <- file.path(source_dir, fname)
        result <- download_file(url, dest, overwrite = FALSE)
        result$location <- "root"
        files_record[[fname]] <- result
        if (!is.null(result$error)) failures <- c(failures, fname)
      }
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

      result <- download_file(url, dest, overwrite = FALSE)
      result$location <- "metadata"
      files_record[[fname]] <- result
      if (!is.null(result$error)) failures <- c(failures, fname)
    }
  }

  # Write provenance record (even on partial failure)
  provenance <- list(
    name = source$name,
    dir_name = source$dir_name,
    origin = "remote",
    landing_url = source$landing_url,
    metadata = source$metadata,
    archives = if (length(archives_record) > 0) archives_record,
    files = files_record,
    created = created,
    last_updated = timestamp_now(),
    attest_version = as.character(utils::packageVersion("attest"))
  )

  jsonlite::write_json(
    provenance,
    file.path(provenance_dir, "provenance.json"),
    pretty = TRUE, auto_unbox = TRUE
  )

  if (cite && length(failures) == 0) {
    att_cite(source, store = store)
  }

  if (length(failures) > 0) {
    cli::cli_alert_warning(
      "Source {.val {source$name}}: {length(failures)} file{?s} failed"
    )
    for (f in failures) {
      err <- files_record[[f]]$error %||% archives_record[[f]]$error
      cli::cli_alert_danger("  {.file {f}}: {err}")
    }
  } else {
    cli::cli_alert_success(
      "Source {.val {source$name}} downloaded to {.path {source_dir}}"
    )
  }
  invisible(provenance)
}
