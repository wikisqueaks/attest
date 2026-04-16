#' Archive a Source's Current Files
#'
#' Copies the current downloaded files, metadata, and provenance record into a
#' timestamped subdirectory under `archive/`. Called by [att_refresh()] before
#' updating files.
#'
#' @param source A source name (character) or [att_source()] object.
#' @param store Path to the provenance store. Defaults to [att_store()].
#' @return The archive directory path, invisibly.
#' @keywords internal
att_archive <- function(source, store = NULL) {
  name <- resolve_source_name(source)
  if (is.null(store)) store <- att_store()

  source_dir <- file.path(store, name)
  if (!dir.exists(source_dir)) {
    cli::cli_abort("Source directory not found: {.path {source_dir}}")
  }

  timestamp <- format(Sys.time(), "%Y%m%d-%H%M%S")
  archive_dir <- file.path(source_dir, "archive", timestamp)
  dir.create(archive_dir, recursive = TRUE, showWarnings = FALSE)

  # Copy all files and subdirectories (excluding the archive directory itself)
  entries <- list.files(source_dir, full.names = TRUE)
  entries <- entries[basename(entries) != "archive"]

  results <- file.copy(entries, archive_dir, recursive = TRUE)
  if (any(!results)) {
    cli::cli_abort("Failed to archive some files in {.val {name}}.")
  }

  cli::cli_alert_success("Archived {.val {name}} to {.path {archive_dir}}")
  invisible(archive_dir)
}


#' Refresh a Source
#'
#' Re-fetches all files for a previously attested source, compares them
#' against the recorded hashes, and updates the local store only if changes are
#' detected. Optionally archives the current files before overwriting.
#'
#' For remote sources (`origin = "remote"`), files are re-downloaded from their
#' original URLs. For local sources (`origin = "local"`), files are re-read
#' from their original `source_path`.
#'
#' For archive sources, the archive is re-downloaded and its hash compared.
#' If unchanged, all extracted files are reported as unchanged. If the archive
#' hash differs, the archive is extracted and individual files are compared
#' and updated.
#'
#' @param source A source name (character) or [att_source()] object.
#' @param store Path to the provenance store. Defaults to [att_store()].
#' @param archive Logical; if `TRUE` (default), archive current files before
#'   updating.
#' @return A data frame summarising the comparison (columns: `file`, `status`,
#'   `old_hash`, `new_hash`, `old_size`, `new_size`), invisibly.
#' @export
att_refresh <- function(source, store = NULL, archive = TRUE) {
  name <- resolve_source_name(source)
  if (is.null(store)) store <- att_store()

  prov_path <- provenance_path(store, name)
  if (!file.exists(prov_path)) {
    cli::cli_abort("No provenance record found for {.val {name}}.")
  }

  prov <- jsonlite::read_json(prov_path)
  origin <- prov$origin %||% "remote"
  has_archives <- !is.null(prov$archives) && length(prov$archives) > 0

  if (origin == "link") {
    cli::cli_abort(c(
      "Source {.val {name}} is a linked source with no files to refresh.",
      "i" = "Use {.fun att_check} to verify the endpoint is still reachable."
    ))
  }

  if (length(prov$files) == 0) {
    cli::cli_alert_info("No files recorded for {.val {name}}.")
    return(invisible(data.frame(
      file = character(0), status = character(0),
      old_hash = character(0), new_hash = character(0),
      old_size = numeric(0), new_size = numeric(0),
      stringsAsFactors = FALSE
    )))
  }

  cli::cli_alert_info(
    "Refreshing {length(prov$files)} file{?s} for {.val {name}}..."
  )

  # Fetch each file to a temp directory and compare hashes
  tmp_dir <- tempfile("att_refresh_")
  dir.create(tmp_dir, recursive = TRUE)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  comparison <- list()
  archive_updates <- list()

  # Process archive sources
  if (has_archives) {
    for (aname in names(prov$archives)) {
      archive_info <- prov$archives[[aname]]

      # Collect file records for this archive
      archive_file_records <- list()
      for (fname in names(prov$files)) {
        if (identical(prov$files[[fname]]$extracted_from, aname)) {
          archive_file_records[[fname]] <- prov$files[[fname]]
        }
      }

      result <- if (!is.null(archive_info$source_path)) {
        refresh_fetch_local_archive(
          aname, archive_info, archive_file_records, tmp_dir
        )
      } else {
        refresh_fetch_archive(
          aname, archive_info, archive_file_records, tmp_dir
        )
      }
      comparison <- c(comparison, result$files)
      archive_updates[[aname]] <- result
    }
  }

  # Process regular (non-archive) files
  regular_files <- names(prov$files)[
    vapply(prov$files, function(fi) is.null(fi$extracted_from), logical(1))
  ]

  if (length(regular_files) > 0) {
    fetch_fn <- if (origin == "local") {
      refresh_fetch_local
    } else {
      refresh_fetch_remote
    }

    for (fname in regular_files) {
      file_info <- prov$files[[fname]]
      tmp_file <- file.path(tmp_dir, fname)
      comparison <- c(comparison, list(fetch_fn(fname, file_info, tmp_file)))
    }
  }

  summary_df <- data.frame(
    file = vapply(comparison, `[[`, character(1), "file"),
    status = vapply(comparison, `[[`, character(1), "status"),
    old_hash = vapply(comparison, `[[`, character(1), "old_hash"),
    new_hash = vapply(comparison, `[[`, character(1), "new_hash"),
    old_size = vapply(comparison, `[[`, numeric(1), "old_size"),
    new_size = vapply(comparison, `[[`, numeric(1), "new_size"),
    stringsAsFactors = FALSE
  )

  n_changed <- sum(summary_df$status == "changed")
  n_error <- sum(summary_df$status == "error")

  # If nothing changed, report and return early
  if (n_changed == 0) {
    cli::cli_alert_success("No changes detected for {.val {name}}.")
    if (n_error > 0) {
      cli::cli_alert_danger(
        "{n_error} file{?s} could not be fetched for comparison."
      )
    }
    return(invisible(summary_df))
  }

  # Changes detected — archive, then update
  if (archive) {
    att_archive(name, store)
  }

  # Use the appropriate timestamp field for the origin type
  ts_field <- if (origin == "local") "registered" else "downloaded"

  source_dir <- file.path(store, name)
  for (entry in comparison) {
    if (entry$status != "changed") next
    if (is.na(entry$tmp_file) || !file.exists(entry$tmp_file)) next

    dest <- if (entry$location == "metadata") {
      file.path(source_dir, "metadata", entry$file)
    } else {
      file.path(source_dir, entry$file)
    }

    file.copy(entry$tmp_file, dest, overwrite = TRUE)

    # Update provenance for this file
    prov$files[[entry$file]]$sha256 <- entry$new_hash
    prov$files[[entry$file]]$size <- entry$new_size
    prov$files[[entry$file]][[ts_field]] <- timestamp_now()
  }

  # Update archive provenance
  archive_ts_field <- if (origin == "local") "registered" else "downloaded"
  for (aname in names(archive_updates)) {
    update <- archive_updates[[aname]]
    if (isTRUE(update$archive_changed) && !is.na(update$archive_hash)) {
      prov$archives[[aname]]$sha256 <- update$archive_hash
      prov$archives[[aname]][[archive_ts_field]] <- timestamp_now()

      # Update size: for remote archives, read from temp download;
      # for local archives, re-stat the source path
      if (!is.null(prov$archives[[aname]]$source_path)) {
        sp <- prov$archives[[aname]]$source_path
        if (file.exists(sp)) {
          prov$archives[[aname]]$size <- file.size(sp)
          prov$archives[[aname]]$source_modified <- format(
            file.mtime(sp), "%Y-%m-%dT%H:%M:%S%z"
          )
        }
      } else {
        tmp_archive <- file.path(tmp_dir, aname)
        if (file.exists(tmp_archive)) {
          prov$archives[[aname]]$size <- file.size(tmp_archive)
        }
      }

      if (!is.null(update$http_headers)) {
        prov$archives[[aname]]$http_etag <- update$http_headers$http_etag
        prov$archives[[aname]]$http_last_modified <-
          update$http_headers$http_last_modified
        prov$archives[[aname]]$http_content_type <-
          update$http_headers$http_content_type
      }
    }
  }

  prov$last_updated <- timestamp_now()

  jsonlite::write_json(
    prov,
    prov_path,
    pretty = TRUE, auto_unbox = TRUE
  )

  # Print summary
  cli::cli_alert_warning("{n_changed} file{?s} changed for {.val {name}}:")
  for (i in seq_len(nrow(summary_df))) {
    row <- summary_df[i, ]
    if (row$status == "changed") {
      old_short <- substr(row$old_hash, 1, 12)
      new_short <- substr(row$new_hash, 1, 12)
      cli::cli_alert_info(
        "{.file {row$file}}: {old_short}... \u2192 {new_short}... ({row$old_size} \u2192 {row$new_size} bytes)"
      )
    } else if (row$status == "error") {
      cli::cli_alert_danger("{.file {row$file}}: could not fetch")
    }
  }

  if (n_error > 0) {
    cli::cli_alert_danger(
      "{n_error} file{?s} could not be fetched for comparison."
    )
  }

  invisible(summary_df)
}
