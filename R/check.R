#' Check Sources for Changes (Lightweight)
#'
#' For remote sources, sends HTTP HEAD requests to the original URLs and
#' compares ETag, Last-Modified, and Content-Length headers against the
#' recorded values. Does not re-download any files.
#'
#' For local sources, stats the original `source_path` and compares file size
#' and modification time against the recorded values. Does not re-read or
#' hash the files (use [att_compare()] for a definitive hash-based check).
#'
#' For archive sources, sends HTTP HEAD to the original archive URL and
#' compares headers. Extracted files are checked at the archive level, not
#' individually.
#'
#' If the server did not provide comparable headers on the original download
#' or the current HEAD request, the file is reported as `"no_comparison"`
#' rather than `"unchanged"`.
#'
#' @param source A source name (character) or [att_source()] object.
#' @param store Path to the provenance store. Defaults to [att_store()].
#' @return An `att_check` object (list with `source` and `files` and
#'   optionally `archives`), invisibly.
#' @export
att_check <- function(source, store = NULL) {
  name <- resolve_source_name(source)
  if (is.null(store)) store <- att_store()

  prov_path <- provenance_path(store, name)
  if (!file.exists(prov_path)) {
    cli::cli_abort("No provenance record found for {.val {name}}.")
  }

  prov <- jsonlite::read_json(prov_path)
  has_archives <- !is.null(prov$archives) && length(prov$archives) > 0
  origin <- prov$origin %||% "remote"

  results <- list()
  archive_results <- list()

  # Check archives
  if (has_archives) {
    for (aname in names(prov$archives)) {
      archive_info <- prov$archives[[aname]]
      if (!is.null(archive_info$source_path)) {
        archive_results[[aname]] <- check_archive_local(aname, archive_info)
      } else {
        archive_results[[aname]] <- check_archive_head(aname, archive_info)
      }
    }
  }

  # Check regular (non-archive) files
  for (fname in names(prov$files)) {
    file_info <- prov$files[[fname]]

    # Skip archive-extracted files (checked at archive level)
    if (!is.null(file_info$extracted_from)) next

    # Local files: stat the original source_path
    if (origin == "local" || !is.null(file_info$source_path)) {
      source_path <- file_info$source_path
      cli::cli_alert("Checking {.file {fname}} at {.path {source_path}}")

      if (is.null(source_path) || !file.exists(source_path)) {
        results[[fname]] <- list(
          status = "source_missing", source_path = source_path
        )
        cli::cli_alert_danger(
          "{.file {fname}}: Source file no longer exists"
        )
        next
      }

      changes <- character(0)

      current_size <- file.size(source_path)
      recorded_size <- as.numeric(file_info$size %||% 0)
      if (current_size != recorded_size) {
        changes <- c(changes, "size changed")
      }

      current_mtime <- format(file.mtime(source_path), "%Y-%m-%dT%H:%M:%S%z")
      recorded_mtime <- file_info$source_modified
      if (!is.null(recorded_mtime) && current_mtime != recorded_mtime) {
        changes <- c(changes, "modified time changed")
      }

      if (length(changes) > 0) {
        results[[fname]] <- list(
          status = "possibly_changed", source_path = source_path,
          changes = changes
        )
        cli::cli_alert_warning(
          "{.file {fname}}: {paste(changes, collapse = ', ')}"
        )
      } else {
        results[[fname]] <- list(
          status = "unchanged", source_path = source_path
        )
        cli::cli_alert_success("{.file {fname}}: No changes detected")
      }
      next
    }

    url <- file_info$url

    cli::cli_alert("Checking {.url {url}}")

    resp <- tryCatch(
      {
        att_request(url) |>
          httr2::req_method("HEAD") |>
          httr2::req_perform()
      },
      error = function(e) {
        cli::cli_alert_warning(
          "Could not reach {.url {url}}: {conditionMessage(e)}"
        )
        NULL
      }
    )

    if (is.null(resp)) {
      results[[fname]] <- list(status = "unreachable", url = url)
      next
    }

    remote_etag <- httr2::resp_header(resp, "ETag")
    remote_modified <- httr2::resp_header(resp, "Last-Modified")
    remote_length <- httr2::resp_header(resp, "Content-Length")

    changes <- character(0)
    compared <- FALSE

    if (!is.null(remote_etag) && !is.null(file_info$http_etag)) {
      compared <- TRUE
      if (remote_etag != file_info$http_etag) {
        changes <- c(changes, "ETag changed")
      }
    }

    if (!is.null(remote_modified) && !is.null(file_info$http_last_modified)) {
      compared <- TRUE
      if (remote_modified != file_info$http_last_modified) {
        changes <- c(changes, "Last-Modified changed")
      }
    }

    if (!is.null(remote_length)) {
      compared <- TRUE
      local_size <- file_info$size %||% 0
      if (as.numeric(remote_length) != local_size) {
        changes <- c(changes, "Content-Length changed")
      }
    }

    if (length(changes) > 0) {
      results[[fname]] <- list(
        status = "possibly_changed", url = url, changes = changes
      )
      cli::cli_alert_warning(
        "{.file {fname}}: {paste(changes, collapse = ', ')}"
      )
    } else if (!compared) {
      results[[fname]] <- list(status = "no_comparison", url = url)
      cli::cli_alert_info(
        "{.file {fname}}: No headers available for comparison"
      )
    } else {
      results[[fname]] <- list(status = "unchanged", url = url)
      cli::cli_alert_success("{.file {fname}}: No changes detected")
    }
  }

  invisible(structure(
    list(
      source = name,
      files = results,
      archives = if (length(archive_results) > 0) archive_results
    ),
    class = "att_check"
  ))
}
