#' Register Local Files
#'
#' Copies local data and metadata files into the store, computes SHA-256
#' hashes, and writes a `provenance.json` record. This is the local-file
#' counterpart to [att_download()].
#'
#' The provenance record marks `origin` as `"local"` and records each file's
#' original absolute path (`source_path`) instead of a URL. Downstream
#' functions ([att_verify()], [att_refresh()]) work with registered sources;
#' [att_check()] will report `"not_remote"` for each file.
#'
#' @section Archive (zip) support:
#'
#' When a path in `data_paths` points to an archive (`.zip`, `.tar.gz`, or
#' `.tgz`), `att_register()` automatically extracts the archive and classifies
#' its contents, just like [att_download()] does for remote archives. In an
#' interactive session, you will be prompted to classify each extracted file as
#' data, metadata, or ignore. In non-interactive sessions, all files default to
#' data unless `classify` is provided.
#'
#' The archive file itself is not copied into the store, but its SHA-256 hash
#' and original path are recorded in provenance for future comparison. Each
#' extracted file records which archive it came from.
#'
#' @param source An [att_source()] object with `data_paths` and/or
#'   `metadata_paths` populated.
#' @param store Path to the provenance store. Defaults to [att_store()].
#' @param move Logical; if `TRUE`, delete the original file after a successful
#'   copy into the store. For archives, the original archive file is deleted
#'   after successful extraction. Ignored when the file is already in the
#'   correct location. Defaults to `FALSE` (copy, keeping the original).
#' @param cite Logical; if `TRUE` (default), generate a BibTeX citation and
#'   append it to `data-sources.bib` in the store root.
#' @param classify Optional named list for non-interactive archive file
#'   classification. Elements `data`, `metadata`, and `ignore` each contain
#'   character vectors of file extensions (e.g.,
#'   `list(data = c(".shp", ".dbf"), metadata = ".xml")`). Only used for
#'   archive paths; ignored for regular files.
#' @return A list containing the full provenance record, invisibly.
#' @export
#' @examples
#' \dontrun{
#' src <- att_source(
#'   name = "local-survey",
#'   data_paths = c(survey = "~/data/survey-2025.csv"),
#'   metadata_paths = c(codebook = "~/data/survey-codebook.pdf"),
#'   title = "Annual Survey 2025",
#'   publisher = "Ministry of Statistics"
#' )
#' att_register(src)
#'
#' # Register a local archive with interactive classification
#' shp <- att_source(
#'   name = "boundaries",
#'   data_paths = c("boundaries.zip" = "~/data/boundaries.zip")
#' )
#' att_register(shp)
#'
#' # Non-interactive archive classification
#' att_register(shp, classify = list(
#'   metadata = c(".xml", ".pdf"),
#'   ignore = ".html"
#' ))
#' }
att_register <- function(source, store = NULL, move = FALSE, cite = TRUE,
                         classify = NULL) {
  if (!inherits(source, "att_source")) {
    cli::cli_abort("{.arg source} must be an {.cls att_source} object.")
  }

  if (!is.null(source$data_urls) || !is.null(source$metadata_urls)) {
    cli::cli_abort(c(
      "{.fun att_register} is for local sources with file paths.",
      "i" = "This source has URLs. Use {.fun att_download} instead."
    ))
  }

  if (length(source$data_paths) == 0 && length(source$metadata_paths) == 0) {
    cli::cli_abort(
      "Source {.val {source$name}} has no {.arg data_paths} or {.arg metadata_paths}."
    )
  }

  if (is.null(store)) store <- att_store()

  source_dir <- file.path(store, source$dir_name)
  provenance_dir <- file.path(source_dir, "_attest")
  existing_prov <- file.path(provenance_dir, "provenance.json")

  if (file.exists(existing_prov)) {
    cli::cli_abort(c(
      "Source {.val {source$name}} has already been registered.",
      "i" = "Use {.fun att_refresh} to check for updates and re-register."
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

  # Copy data files (to source root)
  if (length(source$data_paths) > 0) {
    cli::cli_alert_info(
      "Registering {length(source$data_paths)} data file{?s}..."
    )
    for (i in seq_along(source$data_paths)) {
      path <- source$data_paths[i]
      fname <- derive_local_filename(source$data_paths, i)

      if (is_archive_path(path)) {
        # Archive workflow: extract, classify, place
        archive_result <- process_archive_path(
          path, fname, source_dir, metadata_dir, classify = classify
        )
        archives_record[[fname]] <- archive_result$archive
        files_record <- c(files_record, archive_result$files)
        failures <- c(failures, archive_result$failures)
      } else {
        dest <- file.path(source_dir, fname)
        result <- register_file(path, dest, move = move)
        result$location <- "root"
        files_record[[fname]] <- result
        if (!is.null(result$error)) failures <- c(failures, fname)
      }
    }
  }

  # Copy metadata files (to metadata/ subdirectory)
  if (length(source$metadata_paths) > 0) {
    cli::cli_alert_info(
      "Registering {length(source$metadata_paths)} metadata file{?s}..."
    )
    for (i in seq_along(source$metadata_paths)) {
      path <- source$metadata_paths[i]
      fname <- derive_local_filename(source$metadata_paths, i)
      dest <- file.path(metadata_dir, fname)

      result <- register_file(path, dest, move = move)
      result$location <- "metadata"
      files_record[[fname]] <- result
      if (!is.null(result$error)) failures <- c(failures, fname)
    }
  }

  provenance <- list(
    name = source$name,
    dir_name = source$dir_name,
    origin = "local",
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
    if (move) {
      cli::cli_alert_info("Originals kept because of failures.")
    }
  } else {
    # Delete originals only when move = TRUE and all files succeeded
    if (move) {
      # Delete original archive files
      for (aname in names(archives_record)) {
        rec <- archives_record[[aname]]
        if (!is.null(rec$source_path) && file.exists(rec$source_path)) {
          file.remove(rec$source_path)
          cli::cli_alert_info(
            "Removed original archive: {.path {rec$source_path}}"
          )
        }
      }

      # Delete original regular files
      for (fname in names(files_record)) {
        rec <- files_record[[fname]]
        if (!is.null(rec$extracted_from)) next
        if (isTRUE(rec$same_file)) next

        dest <- resolve_file_path(store, source$dir_name, fname, rec)
        if (identical(att_hash(rec$source_path), att_hash(dest))) {
          file.remove(rec$source_path)
          cli::cli_alert_info("Removed original: {.path {rec$source_path}}")
        } else {
          cli::cli_alert_warning(
            "Hash mismatch after copy; keeping original: {.path {rec$source_path}}"
          )
        }
      }
    }

    cli::cli_alert_success(
      "Source {.val {source$name}} registered to {.path {source_dir}}"
    )
  }
  invisible(provenance)
}
