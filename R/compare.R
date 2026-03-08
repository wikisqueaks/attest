#' Compare Local Files to Remote Source
#'
#' Re-downloads each file from the original URL to a temporary location,
#' computes its SHA-256 hash, and compares it to the recorded hash in the
#' provenance record. This gives a definitive answer about whether the remote
#' source has changed since the original download. Temporary files are cleaned
#' up automatically.
#'
#' @param source A source name (character) or [acq_source()] object.
#' @param store Path to the store. Defaults to [acq_store()].
#' @return A data frame with columns `file`, `status` (`"match"` or
#'   `"changed"`), `recorded_hash`, and `remote_hash`, invisibly.
#' @export
#' @examples
#' \dontrun{
#' acq_compare("my-source")
#' }
acq_compare <- function(source, store = NULL) {
  name <- resolve_source_name(source)
  if (is.null(store)) store <- acq_store()

  prov_path <- provenance_path(store, name)
  if (!file.exists(prov_path)) {
    cli::cli_abort("No provenance record found for {.val {name}}.")
  }

  prov <- jsonlite::read_json(prov_path)

  if (length(prov$files) == 0) {
    cli::cli_alert_info("No files recorded for {.val {name}}")
    return(invisible(data.frame(
      file = character(0), status = character(0),
      recorded_hash = character(0), remote_hash = character(0),
      stringsAsFactors = FALSE
    )))
  }

  cli::cli_alert_info(
    "Comparing {length(prov$files)} file{?s} for {.val {name}}..."
  )

  results <- lapply(names(prov$files), function(fname) {
    file_info <- prov$files[[fname]]
    url <- file_info$url
    recorded_hash <- file_info$sha256 %||% NA_character_

    cli::cli_alert("Fetching {.file {fname}}")

    tmp <- tempfile(fileext = paste0("_", fname))
    on.exit(unlink(tmp), add = TRUE)

    remote_hash <- tryCatch(
      {
        resp <- acq_request(url) |> httr2::req_perform()
        writeBin(httr2::resp_body_raw(resp), tmp)
        acq_hash(tmp)
      },
      error = function(e) {
        cli::cli_alert_danger(
          "Failed to fetch {.file {fname}}: {conditionMessage(e)}"
        )
        NA_character_
      }
    )

    if (is.na(remote_hash)) {
      status <- "error"
      cli::cli_alert_danger("{.file {fname}}: could not fetch remote file")
    } else if (identical(recorded_hash, remote_hash)) {
      status <- "match"
      cli::cli_alert_success("{.file {fname}}: matches remote")
    } else {
      status <- "changed"
      cli::cli_alert_warning("{.file {fname}}: remote has changed")
    }

    data.frame(
      file = fname, status = status,
      recorded_hash = recorded_hash, remote_hash = remote_hash,
      stringsAsFactors = FALSE
    )
  })

  result <- do.call(rbind, results)

  n_changed <- sum(result$status == "changed")
  n_error <- sum(result$status == "error")
  if (n_changed == 0 && n_error == 0) {
    cli::cli_alert_success("All files match the remote source")
  } else {
    if (n_changed > 0) {
      cli::cli_alert_warning("{n_changed} file{?s} changed since download")
    }
    if (n_error > 0) {
      cli::cli_alert_danger("{n_error} file{?s} could not be checked")
    }
  }

  invisible(result)
}
