#' Generate a Provenance Report
#'
#' Iterates over every source in the store, verifies file integrity, and
#' compiles a comprehensive provenance report. The report includes source
#' metadata, file inventories with recorded and verified hashes, archive
#' information, and an overall verification summary.
#'
#' The report is written as a JSON file and returned as a list. Attach
#' this to a paper or project submission as evidence of data provenance.
#'
#' @param store Path to the provenance store. Defaults to [att_store()].
#' @param output Path for the output JSON file. Defaults to
#'   `provenance-report.json` in the store root. Set to `NULL` to skip
#'   writing.
#' @return An `att_report` object (list), invisibly.
#' @export
#' @examples
#' \dontrun{
#' att_prove()
#' att_prove(output = "reports/provenance.json")
#' }
att_prove <- function(store = NULL, output = NULL) {
  if (is.null(store)) store <- att_store()

  if (!dir.exists(store)) {
    cli::cli_abort("Store directory not found at {.path {store}}.")
  }

  # Discover all sources
  prov_files <- list.files(
    store,
    pattern = "^provenance\\.json$",
    recursive = TRUE,
    full.names = TRUE
  )
  prov_files <- prov_files[grepl("_attest", prov_files, fixed = TRUE)]

  if (length(prov_files) == 0) {
    cli::cli_abort("No sources found in {.path {store}}.")
  }

  cli::cli_alert_info(
    "Generating provenance report for {length(prov_files)} source{?s}..."
  )

  sources <- list()
  total_files <- 0L
  total_verified <- 0L
  total_failed <- 0L

  for (pf in prov_files) {
    prov <- jsonlite::read_json(pf)
    name <- prov$name %||% basename(dirname(dirname(pf)))

    cli::cli_alert("Verifying {.val {name}}...")

    # Verify each file silently, collect results
    verification <- verify_source_silent(prov, store, name)

    n_files <- length(verification$files)
    n_ok <- sum(vapply(verification$files, function(f) {
      identical(f$status, "ok")
    }, logical(1)))
    n_failed <- n_files - n_ok

    total_files <- total_files + n_files
    total_verified <- total_verified + n_ok
    total_failed <- total_failed + n_failed

    # Build file inventory
    file_inventory <- lapply(names(verification$files), function(fname) {
      v <- verification$files[[fname]]
      fi <- prov$files[[fname]]
      entry <- list(
        file = fname,
        location = fi$location %||% "root",
        size = fi$size %||% NULL,
        recorded_hash = v$expected,
        verified_hash = v$actual,
        status = v$status
      )
      if (!is.null(fi$extracted_from)) {
        entry$extracted_from <- fi$extracted_from
      }
      if (!is.null(fi$url)) {
        entry$url <- fi$url
      }
      entry
    })

    source_entry <- list(
      name = name,
      origin = prov$origin %||% "remote",
      landing_url = prov$landing_url,
      metadata = prov$metadata,
      created = prov$created,
      last_updated = prov$last_updated,
      verified = verification$all_ok,
      files = file_inventory
    )

    if (!is.null(prov$archives) && length(prov$archives) > 0) {
      source_entry$archives <- prov$archives
    }

    sources[[name]] <- source_entry

    if (verification$all_ok) {
      cli::cli_alert_success("{.val {name}}: {n_files} file{?s} verified")
    } else {
      cli::cli_alert_warning(
        "{.val {name}}: {n_failed} of {n_files} file{?s} failed verification"
      )
    }
  }

  report <- list(
    report_type = "attest_provenance_report",
    generated = timestamp_now(),
    attest_version = as.character(utils::packageVersion("attest")),
    store = normalizePath(store, winslash = "/"),
    summary = list(
      total_sources = length(sources),
      total_files = total_files,
      files_verified = total_verified,
      files_failed = total_failed,
      all_verified = total_failed == 0L
    ),
    sources = sources
  )

  # Console summary
  cli::cli_text("")
  cli::cli_rule("Provenance Report")
  cli::cli_alert_info(
    "{length(sources)} source{?s}, {total_files} file{?s}"
  )
  if (total_failed == 0L) {
    cli::cli_alert_success("All files verified")
  } else {
    cli::cli_alert_danger(
      "{total_failed} file{?s} failed verification"
    )
  }

  # Write report
  if (is.null(output)) {
    output <- file.path(store, "provenance-report.json")
  }

  if (!is.null(output)) {
    dir.create(dirname(output), recursive = TRUE, showWarnings = FALSE)
    jsonlite::write_json(
      report, output,
      pretty = TRUE, auto_unbox = TRUE
    )
    cli::cli_alert_success("Report written to {.path {output}}")
  }

  invisible(structure(report, class = "att_report"))
}


#' @export
print.att_report <- function(x, ...) {
  cli::cli_rule("Provenance Report")
  cli::cli_text("Generated: {x$generated}")
  cli::cli_text("attest version: {x$attest_version}")
  cli::cli_text("Store: {.path {x$store}}")
  cli::cli_text("")

  s <- x$summary
  if (s$all_verified) {
    cli::cli_alert_success(
      "{s$total_sources} source{?s}, {s$total_files} file{?s} \u2014 all verified"
    )
  } else {
    cli::cli_alert_danger(
      "{s$total_sources} source{?s}, {s$total_files} file{?s} \u2014 {s$files_failed} failed"
    )
  }
  cli::cli_text("")

  for (src in x$sources) {
    status_icon <- if (src$verified) "\u2714" else "\u2716"
    cli::cli_rule("{status_icon} {src$name}")

    meta <- src$metadata
    if (!is.null(meta$title)) cli::cli_text("  Title: {meta$title}")
    if (!is.null(meta$publisher)) cli::cli_text("  Publisher: {meta$publisher}")
    if (!is.null(meta$year)) cli::cli_text("  Year: {meta$year}")
    if (!is.null(src$landing_url)) {
      cli::cli_text("  URL: {.url {src$landing_url}}")
    }
    cli::cli_text("  Origin: {src$origin}")
    cli::cli_text("  Created: {src$created}")

    if (!is.null(src$archives)) {
      for (aname in names(src$archives)) {
        a <- src$archives[[aname]]
        cli::cli_text("  Archive: {.file {aname}} (SHA-256: {substr(a$sha256, 1, 16)}\u2026)")
      }
    }

    cli::cli_text("")
    cli::cli_text("  {.strong Files}:")

    for (f in src$files) {
      hash_short <- if (!is.na(f$verified_hash)) {
        substr(f$verified_hash, 1, 16)
      } else {
        "n/a"
      }
      size_label <- if (!is.null(f$size)) format_size(f$size) else "?"

      icon <- switch(f$status,
        ok = "\u2714",
        changed = "\u2716",
        missing = "?",
        "\u2716"
      )

      loc <- if (f$location == "metadata") " [metadata]" else ""
      from <- if (!is.null(f$extracted_from)) {
        paste0(" \u2190 ", f$extracted_from)
      } else {
        ""
      }

      cli::cli_text(
        "  {icon} {f$file} ({size_label}) {hash_short}\u2026{loc}{from}"
      )
    }
    cli::cli_text("")
  }

  invisible(x)
}


#' Verify a source without printing per-file messages
#'
#' Same logic as att_verify() but suppresses per-file cli output.
#' Returns the same structure.
#' @noRd
verify_source_silent <- function(prov, store, name) {
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
    } else {
      current_hash <- att_hash(file_path)
      if (identical(current_hash, recorded_hash)) {
        results[[fname]] <- list(
          status = "ok", expected = recorded_hash, actual = current_hash
        )
      } else {
        results[[fname]] <- list(
          status = "changed", expected = recorded_hash, actual = current_hash
        )
        all_ok <- FALSE
      }
    }
  }

  list(source = name, all_ok = all_ok, files = results)
}
