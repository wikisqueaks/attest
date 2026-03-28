#' Import Sources from a Manifest
#'
#' Reads an [att_export()] manifest and replays each source acquisition
#' in the target store. Remote sources are re-downloaded with
#' [att_download()]; local sources are re-registered with [att_register()].
#'
#' Sources that already exist in the target store (i.e., their
#' `provenance.json` is already present) are skipped. Local sources whose
#' original paths no longer exist are also skipped with a warning.
#'
#' @param path Path to the manifest JSON file (created by [att_export()]).
#' @param store Path to the target provenance store. Defaults to
#'   [att_store()].
#' @return A data frame with columns `source`, `origin`, and `status`,
#'   invisibly. Status values: `"downloaded"`, `"registered"`, `"skipped"`,
#'   `"failed"`.
#' @seealso [att_export()] to create a manifest.
#' @export
#' @examples
#' \dontrun{
#' att_import("attest-manifest.json")
#' att_import("attest-manifest.json", store = "data/raw")
#' }
att_import <- function(path, store = NULL) {
  if (!file.exists(path)) {
    cli::cli_abort("Manifest not found at {.path {path}}.")
  }

  manifest <- jsonlite::read_json(path)

  if (!identical(manifest$manifest_type, "attest_manifest")) {
    cli::cli_abort(c(
      "File does not appear to be an attest manifest.",
      "i" = "Expected {.val manifest_type} to be {.val attest_manifest}."
    ))
  }

  if (is.null(store)) store <- att_store()

  sources <- manifest$sources %||% list()

  if (length(sources) == 0) {
    cli::cli_alert_info("Manifest contains no sources.")
    return(invisible(data.frame(
      source = character(0),
      origin = character(0),
      status = character(0),
      stringsAsFactors = FALSE
    )))
  }

  cli::cli_alert_info(
    "Importing {length(sources)} source{?s} from manifest..."
  )

  results <- vector("list", length(sources))

  for (i in seq_along(sources)) {
    entry <- sources[[i]]
    name <- entry$name
    origin <- entry$origin %||% "remote"

    # Check if already exists
    source_dir <- file.path(store, gsub("[^a-zA-Z0-9_-]", "-", name))
    existing_prov <- file.path(source_dir, "_attest", "provenance.json")

    if (file.exists(existing_prov)) {
      cli::cli_alert_info(
        "Skipping {.val {name}}: already exists in store"
      )
      results[[i]] <- list(
        source = name, origin = origin, status = "skipped"
      )
      next
    }

    if (origin == "remote") {
      results[[i]] <- import_remote_source(entry, store)
    } else {
      results[[i]] <- import_local_source(entry, store)
    }
  }

  summary_df <- do.call(rbind, lapply(results, as.data.frame,
    stringsAsFactors = FALSE
  ))

  n_ok <- sum(summary_df$status %in% c("downloaded", "registered"))
  n_skip <- sum(summary_df$status == "skipped")
  n_fail <- sum(summary_df$status == "failed")

  cli::cli_text("")
  cli::cli_rule("Import complete")
  if (n_ok > 0) {
    cli::cli_alert_success("{n_ok} source{?s} acquired")
  }
  if (n_skip > 0) {
    cli::cli_alert_info("{n_skip} source{?s} skipped (already in store)")
  }
  if (n_fail > 0) {
    cli::cli_alert_danger("{n_fail} source{?s} failed")
  }

  invisible(summary_df)
}


#' Import a single remote source from a manifest entry
#' @noRd
import_remote_source <- function(entry, store) {
  name <- entry$name

  data_urls <- unlist_named(entry$data_urls)
  metadata_urls <- unlist_named(entry$metadata_urls)
  metadata <- entry$metadata %||% list()

  src <- att_source(
    name = name,
    landing_url = entry$landing_url,
    data_urls = data_urls,
    metadata_urls = metadata_urls,
    title = metadata$title,
    publisher = metadata$publisher,
    year = metadata$year,
    author = metadata$author,
    format = metadata$format,
    description = metadata$description
  )

  classify <- unlist_classify(entry$classify)

  status <- tryCatch(
    {
      att_download(src, store = store, classify = classify)
      "downloaded"
    },
    error = function(e) {
      cli::cli_alert_danger(
        "Failed to import {.val {name}}: {conditionMessage(e)}"
      )
      "failed"
    }
  )

  list(source = name, origin = "remote", status = status)
}


#' Import a single local source from a manifest entry
#' @noRd
import_local_source <- function(entry, store) {
  name <- entry$name

  data_paths <- unlist_named(entry$data_paths)
  metadata_paths <- unlist_named(entry$metadata_paths)
  metadata <- entry$metadata %||% list()

  # Check that paths exist before attempting registration
  all_paths <- c(data_paths, metadata_paths)
  missing <- all_paths[!file.exists(all_paths)]

  if (length(missing) > 0) {
    cli::cli_alert_warning(
      "Skipping {.val {name}}: {length(missing)} file{?s} not found"
    )
    for (m in missing) {
      cli::cli_alert_danger("  {.path {m}}")
    }
    return(list(source = name, origin = "local", status = "failed"))
  }

  src <- att_source(
    name = name,
    landing_url = entry$landing_url,
    data_paths = data_paths,
    metadata_paths = metadata_paths,
    title = metadata$title,
    publisher = metadata$publisher,
    year = metadata$year,
    author = metadata$author,
    format = metadata$format,
    description = metadata$description
  )

  status <- tryCatch(
    {
      att_register(src, store = store)
      "registered"
    },
    error = function(e) {
      cli::cli_alert_danger(
        "Failed to import {.val {name}}: {conditionMessage(e)}"
      )
      "failed"
    }
  )

  list(source = name, origin = "local", status = status)
}


#' Convert a JSON named list to a named character vector
#'
#' Manifest JSON stores URL/path mappings as objects (`{"name": "value"}`).
#' This converts them to named character vectors as expected by
#' [att_source()].
#' @noRd
unlist_named <- function(x) {
  if (is.null(x) || length(x) == 0) return(NULL)
  vals <- vapply(x, function(v) as.character(v), character(1))
  nms <- names(x)
  stats::setNames(vals, nms)
}


#' Convert a JSON classify object to the list format expected by
#' att_download()
#' @noRd
unlist_classify <- function(x) {
  if (is.null(x)) return(NULL)
  result <- list()
  for (role in c("data", "metadata", "ignore")) {
    if (!is.null(x[[role]])) {
      result[[role]] <- vapply(x[[role]], as.character, character(1))
    }
  }
  if (length(result) == 0) return(NULL)
  result
}
