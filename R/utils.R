# Internal utilities -------------------------------------------------------

#' Resolve a source argument to a directory name
#'
#' Accepts either a string (treated as the directory name) or an
#' `acq_source` object.
#' @noRd
resolve_source_name <- function(source) {
  if (is.character(source) && length(source) == 1) {
    return(source)
  }
  if (inherits(source, "acq_source")) {
    return(source$dir_name)
  }

  cli::cli_abort("{.arg source} must be a source name or {.cls acq_source} object.")
}

#' ISO 8601 timestamp
#' @noRd
timestamp_now <- function() {
  format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
}

#' Build a base httr2 request with user-agent and timeout
#' @noRd
acq_request <- function(url) {
  httr2::request(url) |>
    httr2::req_user_agent("acquire (R package; data provenance tracking)") |>
    httr2::req_timeout(seconds = 300)
}

#' Download a single file and record metadata
#'
#' Wraps the HTTP request in tryCatch so that a failed download returns an
#' error record rather than stopping execution.
#' @noRd
download_file <- function(url, dest, overwrite = FALSE) {
  if (file.exists(dest) && !overwrite) {
    cli::cli_alert_warning("Skipping existing file: {.file {basename(dest)}}")
    return(list(
      url = url,
      downloaded = NA_character_,
      size = file.size(dest),
      sha256 = acq_hash(dest),
      skipped = TRUE,
      error = NULL
    ))
  }

  cli::cli_alert("Downloading {.url {url}}")

  tryCatch(
    {
      resp <- acq_request(url) |>
        httr2::req_perform()

      writeBin(httr2::resp_body_raw(resp), dest)

      list(
        url = url,
        downloaded = timestamp_now(),
        size = file.size(dest),
        sha256 = acq_hash(dest),
        http_etag = httr2::resp_header(resp, "ETag"),
        http_last_modified = httr2::resp_header(resp, "Last-Modified"),
        http_content_type = httr2::resp_header(resp, "Content-Type"),
        skipped = FALSE,
        error = NULL
      )
    },
    error = function(e) {
      cli::cli_alert_danger(
        "Failed to download {.url {url}}: {conditionMessage(e)}"
      )
      list(
        url = url,
        downloaded = NULL,
        size = NULL,
        sha256 = NULL,
        skipped = FALSE,
        error = conditionMessage(e)
      )
    }
  )
}


#' Resolve the path to a source's provenance.json
#' @noRd
provenance_path <- function(store, name) {
  file.path(store, name, "_acquire", "provenance.json")
}

#' Resolve the local path of a file from its provenance record
#' @noRd
resolve_file_path <- function(store, name, fname, file_info) {
  location <- file_info$location %||% "root"
  if (location == "metadata") {
    file.path(store, name, "metadata", fname)
  } else {
    file.path(store, name, fname)
  }
}

#' Derive filenames from URLs, using names if provided
#' @noRd
derive_filename <- function(urls, index) {
  nm <- names(urls)[index]
  if (!is.null(nm) && nchar(nm) > 0) {
    return(nm)
  }
  # Strip query strings and fragment identifiers before taking basename
  clean_url <- sub("[?#].*$", "", urls[index])
  basename(clean_url)
}

#' Build a BibTeX entry from named fields
#' @noRd
build_bib_entry <- function(key, type = "misc", fields) {
  fields <- fields[!vapply(fields, is.null, logical(1))]
  lines <- paste0("  ", names(fields), " = {", fields, "}")
  body <- paste(lines, collapse = ",\n")
  paste0("@", type, "{", key, ",\n", body, "\n}")
}

#' Update a .bib file: replace existing entry with same key, or append
#' @noRd
update_bib_file <- function(bib_path, key, bib_text) {
  if (file.exists(bib_path)) {
    existing <- readLines(bib_path, warn = FALSE)
    content <- paste(existing, collapse = "\n")

    # Remove existing entry with this key (match across multiple lines)
    escaped_key <- gsub("([.])", "\\\\\\1", key)
    pattern <- paste0("@\\w+\\{", escaped_key, ",(?s).*?\\n\\}")
    content <- gsub(pattern, "", content, perl = TRUE)

    # Clean up multiple blank lines
    content <- gsub("\n{3,}", "\n\n", trimws(content))

    if (nchar(content) > 0) {
      writeLines(paste0(content, "\n\n", bib_text), bib_path)
    } else {
      writeLines(bib_text, bib_path)
    }
  } else {
    writeLines(bib_text, bib_path)
  }
}
