#' Verify Local File Integrity
#'
#' Re-computes SHA-256 hashes of downloaded files and compares them to the
#' recorded hashes in `provenance.json`. Reports any mismatches or missing
#' files.
#'
#' @param source A source name (character) or [att_source()] object.
#' @param store Path to the provenance store. Defaults to [att_store()].
#' @return An `att_verification` object (list with `source`, `all_ok`, and
#'   `files`), invisibly.
#' @export
#' @examples
#' \dontrun{
#' att_verify("my-source")
#' }
att_verify <- function(source, store = NULL) {
  name <- resolve_source_name(source)
  if (is.null(store)) store <- att_store()

  prov_path <- provenance_path(store, name)
  if (!file.exists(prov_path)) {
    cli::cli_abort("No provenance record found for {.val {name}}.")
  }

  prov <- jsonlite::read_json(prov_path)

  results <- list()
  all_ok <- TRUE

  for (fname in names(prov$files)) {
    file_info <- prov$files[[fname]]
    file_path <- resolve_file_path(store, name, fname, file_info)
    recorded_hash <- file_info$sha256

    if (!file.exists(file_path)) {
      results[[fname]] <- list(
        status = "missing", expected = recorded_hash, actual = NA_character_
      )
      all_ok <- FALSE
      cli::cli_alert_danger("{.file {fname}}: MISSING")
    } else {
      current_hash <- att_hash(file_path)
      if (identical(current_hash, recorded_hash)) {
        results[[fname]] <- list(
          status = "ok", expected = recorded_hash, actual = current_hash
        )
        cli::cli_alert_success("{.file {fname}}: OK")
      } else {
        results[[fname]] <- list(
          status = "changed", expected = recorded_hash, actual = current_hash
        )
        all_ok <- FALSE
        cli::cli_alert_danger("{.file {fname}}: CHANGED")
      }
    }
  }

  if (all_ok) {
    cli::cli_alert_success("All files verified for {.val {name}}")
  } else {
    cli::cli_alert_warning(
      "Source {.val {name}} has been modified since download"
    )
  }

  invisible(structure(
    list(source = name, all_ok = all_ok, files = results),
    class = "att_verification"
  ))
}
