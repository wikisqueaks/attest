#' Register Local Files
#'
#' Copies local data and metadata files into the store, computes SHA-256
#' hashes, and writes a `provenance.json` record. This is the local-file
#' counterpart to [acq_download()].
#'
#' The provenance record marks `origin` as `"local"` and records each file's
#' original absolute path (`source_path`) instead of a URL. Downstream
#' functions ([acq_verify()], [acq_refresh()]) work with registered sources;
#' [acq_check()] will report `"not_remote"` for each file.
#'
#' @param source An [acq_source()] object with `data_paths` and/or
#'   `metadata_paths` populated.
#' @param store Path to the provenance store. Defaults to [acq_store()].
#' @param move Logical; if `TRUE`, delete the original file after a successful
#'   copy into the store. Ignored when the file is already in the correct
#'   location. Defaults to `FALSE` (copy, keeping the original).
#' @param cite Logical; if `TRUE` (default), generate a BibTeX citation and
#'   append it to `data-sources.bib` in the store root.
#' @return A list containing the full provenance record, invisibly.
#' @export
#' @examples
#' \dontrun{
#' src <- acq_source(
#'   name = "local-survey",
#'   data_paths = c(survey = "~/data/survey-2025.csv"),
#'   metadata_paths = c(codebook = "~/data/survey-codebook.pdf"),
#'   title = "Annual Survey 2025",
#'   publisher = "Ministry of Statistics"
#' )
#' acq_register(src)
#' }
acq_register <- function(source, store = NULL, move = FALSE, cite = TRUE) {
  if (!inherits(source, "acq_source")) {
    cli::cli_abort("{.arg source} must be an {.cls acq_source} object.")
  }

  if (!is.null(source$data_urls) || !is.null(source$metadata_urls)) {
    cli::cli_abort(c(
      "{.fun acq_register} is for local sources with file paths.",
      "i" = "This source has URLs. Use {.fun acq_download} instead."
    ))
  }

  if (length(source$data_paths) == 0 && length(source$metadata_paths) == 0) {
    cli::cli_abort(
      "Source {.val {source$name}} has no {.arg data_paths} or {.arg metadata_paths}."
    )
  }

  if (is.null(store)) store <- acq_store()

  source_dir <- file.path(store, source$dir_name)
  provenance_dir <- file.path(source_dir, "_acquire")
  existing_prov <- file.path(provenance_dir, "provenance.json")

  if (file.exists(existing_prov)) {
    cli::cli_abort(c(
      "Source {.val {source$name}} has already been registered.",
      "i" = "Use {.fun acq_refresh} to check for updates and re-register."
    ))
  }

  metadata_dir <- file.path(source_dir, "metadata")

  dir.create(source_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(metadata_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(provenance_dir, recursive = TRUE, showWarnings = FALSE)

  created <- timestamp_now()

  files_record <- list()
  failures <- character(0)

  # Copy data files (to source root)
  if (length(source$data_paths) > 0) {
    cli::cli_alert_info(
      "Registering {length(source$data_paths)} data file{?s}..."
    )
    for (i in seq_along(source$data_paths)) {
      path <- source$data_paths[i]
      fname <- derive_local_filename(source$data_paths, i)
      dest <- file.path(source_dir, fname)

      result <- register_file(path, dest, move = move)
      result$location <- "root"
      files_record[[fname]] <- result
      if (!is.null(result$error)) failures <- c(failures, fname)
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
    if (move) {
      cli::cli_alert_info("Originals kept because of failures.")
    }
  } else {
    # Delete originals only when move = TRUE and all files succeeded
    if (move) {
      for (fname in names(files_record)) {
        rec <- files_record[[fname]]
        if (rec$same_file) next

        dest <- resolve_file_path(store, source$dir_name, fname, rec)
        if (identical(acq_hash(rec$source_path), acq_hash(dest))) {
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
