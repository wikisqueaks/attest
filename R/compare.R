#' Compare Local Files to Their Original Source
#'
#' Re-fetches each file from the original source to a temporary location,
#' computes its SHA-256 hash, and compares it to the recorded hash in the
#' provenance record. This gives a definitive answer about whether the source
#' has changed since the original acquisition. Temporary files are cleaned
#' up automatically.
#'
#' For remote sources (`origin = "remote"`), files are re-downloaded from their
#' original URLs. For local sources (`origin = "local"`), files are re-read
#' from their original `source_path`.
#'
#' For archive sources, the archive is re-downloaded and its hash compared. If
#' the archive hash matches, all extracted files are reported as `"match"`. If
#' the archive hash differs, all extracted files are reported as `"changed"`.
#'
#' @param source A source name (character) or [att_source()] object.
#' @param store Path to the store. Defaults to [att_store()].
#' @return A data frame with columns `file`, `status` (`"match"`,
#'   `"changed"`, or `"error"`), `recorded_hash`, and `source_hash`,
#'   invisibly.
#' @export
#' @examples
#' \dontrun{
#' att_compare("my-source")
#' }
att_compare <- function(source, store = NULL) {
  name <- resolve_source_name(source)
  if (is.null(store)) store <- att_store()

  prov_path <- provenance_path(store, name)
  if (!file.exists(prov_path)) {
    cli::cli_abort("No provenance record found for {.val {name}}.")
  }

  prov <- jsonlite::read_json(prov_path)
  origin <- prov$origin %||% "remote"
  has_archives <- !is.null(prov$archives) && length(prov$archives) > 0

  if (length(prov$files) == 0) {
    cli::cli_alert_info("No files recorded for {.val {name}}")
    return(invisible(data.frame(
      file = character(0), status = character(0),
      recorded_hash = character(0), source_hash = character(0),
      stringsAsFactors = FALSE
    )))
  }

  results <- list()

  # Compare archive sources
  if (has_archives) {
    for (aname in names(prov$archives)) {
      archive_info <- prov$archives[[aname]]

      # Find files extracted from this archive
      archive_file_names <- names(prov$files)[
        vapply(prov$files, function(fi) {
          identical(fi$extracted_from, aname)
        }, logical(1))
      ]

      archive_result <- if (!is.null(archive_info$source_path)) {
        compare_fetch_local_archive(aname, archive_info)
      } else {
        compare_fetch_archive(aname, archive_info)
      }

      if (archive_result$status == "match") {
        cli::cli_alert_success(
          "Archive {.file {aname}}: unchanged ({length(archive_file_names)} file{?s})"
        )
        for (fname in archive_file_names) {
          recorded <- prov$files[[fname]]$sha256 %||% NA_character_
          results <- c(results, list(data.frame(
            file = fname, status = "match",
            recorded_hash = recorded, source_hash = recorded,
            stringsAsFactors = FALSE
          )))
        }
      } else if (archive_result$status == "changed") {
        cli::cli_alert_warning(
          "Archive {.file {aname}}: source has changed"
        )
        for (fname in archive_file_names) {
          results <- c(results, list(data.frame(
            file = fname, status = "changed",
            recorded_hash = prov$files[[fname]]$sha256 %||% NA_character_,
            source_hash = NA_character_,
            stringsAsFactors = FALSE
          )))
        }
      } else {
        cli::cli_alert_danger(
          "Archive {.file {aname}}: could not fetch"
        )
        for (fname in archive_file_names) {
          results <- c(results, list(data.frame(
            file = fname, status = "error",
            recorded_hash = prov$files[[fname]]$sha256 %||% NA_character_,
            source_hash = NA_character_,
            stringsAsFactors = FALSE
          )))
        }
      }
    }
  }

  # Compare regular (non-archive) files
  regular_files <- names(prov$files)[
    vapply(prov$files, function(fi) is.null(fi$extracted_from), logical(1))
  ]

  if (length(regular_files) > 0) {
    cli::cli_alert_info(
      "Comparing {length(regular_files)} file{?s} for {.val {name}}..."
    )
  }

  for (fname in regular_files) {
    file_info <- prov$files[[fname]]
    recorded_hash <- file_info$sha256 %||% NA_character_

    tmp <- tempfile(fileext = paste0("_", fname))
    on.exit(unlink(tmp), add = TRUE)

    source_hash <- if (origin == "local") {
      compare_fetch_local(fname, file_info, tmp)
    } else {
      compare_fetch_remote(fname, file_info, tmp)
    }

    if (is.na(source_hash)) {
      status <- "error"
      cli::cli_alert_danger("{.file {fname}}: could not fetch source file")
    } else if (identical(recorded_hash, source_hash)) {
      status <- "match"
      cli::cli_alert_success("{.file {fname}}: matches source")
    } else {
      status <- "changed"
      cli::cli_alert_warning("{.file {fname}}: source has changed")
    }

    results <- c(results, list(data.frame(
      file = fname, status = status,
      recorded_hash = recorded_hash, source_hash = source_hash,
      stringsAsFactors = FALSE
    )))
  }

  result <- do.call(rbind, results)

  n_changed <- sum(result$status == "changed")
  n_error <- sum(result$status == "error")
  if (n_changed == 0 && n_error == 0) {
    cli::cli_alert_success("All files match the source")
  } else {
    if (n_changed > 0) {
      cli::cli_alert_warning("{n_changed} file{?s} changed since acquisition")
    }
    if (n_error > 0) {
      cli::cli_alert_danger("{n_error} file{?s} could not be checked")
    }
  }

  invisible(result)
}
