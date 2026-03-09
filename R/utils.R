# Internal utilities -------------------------------------------------------

#' Resolve a source argument to a directory name
#'
#' Accepts either a string (treated as the directory name) or an
#' `att_source` object.
#' @noRd
resolve_source_name <- function(source) {
  if (is.character(source) && length(source) == 1) {
    return(source)
  }
  if (inherits(source, "att_source")) {
    return(source$dir_name)
  }

  cli::cli_abort("{.arg source} must be a source name or {.cls att_source} object.")
}

#' ISO 8601 timestamp
#' @noRd
timestamp_now <- function() {
  format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
}

#' Build a base httr2 request with user-agent and timeout
#' @noRd
att_request <- function(url) {
  httr2::request(url) |>
    httr2::req_user_agent("attest (R package; data provenance tracking)") |>
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
      sha256 = att_hash(dest),
      skipped = TRUE,
      error = NULL
    ))
  }

  cli::cli_alert("Downloading {.url {url}}")

  tryCatch(
    {
      resp <- att_request(url) |>
        httr2::req_perform()

      writeBin(httr2::resp_body_raw(resp), dest)

      list(
        url = url,
        downloaded = timestamp_now(),
        size = file.size(dest),
        sha256 = att_hash(dest),
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

#' Register a local file into the store
#'
#' The local-file counterpart to `download_file()`. Ensures the file exists at
#' `dest` (copying if needed), records the original absolute path, and computes
#' a hash. When `move = TRUE`, the original is deleted after a successful copy
#' (only if source and destination differ).
#' @noRd
register_file <- function(path, dest, move = FALSE) {
  abs_path <- normalizePath(path, mustWork = FALSE)
  abs_dest <- normalizePath(dest, mustWork = FALSE)
  same_file <- identical(abs_path, abs_dest)

  if (!file.exists(abs_path)) {
    cli::cli_alert_danger("File not found: {.path {abs_path}}")
    return(list(
      source_path = abs_path,
      registered = NULL,
      size = NULL,
      sha256 = NULL,
      skipped = FALSE,
      error = paste0("File not found: ", abs_path)
    ))
  }

  tryCatch(
    {
      if (same_file) {
        cli::cli_alert("Registering {.file {basename(abs_path)}} (already in place)")
      } else {
        action <- if (move) "Moving" else "Copying"
        cli::cli_alert("{action} {.path {abs_path}}")
        file.copy(abs_path, dest, overwrite = FALSE)
      }

      list(
        source_path = abs_path,
        registered = timestamp_now(),
        size = file.size(dest),
        sha256 = att_hash(dest),
        source_modified = format(file.mtime(dest), "%Y-%m-%dT%H:%M:%S%z"),
        skipped = FALSE,
        same_file = same_file,
        error = NULL
      )
    },
    error = function(e) {
      cli::cli_alert_danger(
        "Failed to register {.path {abs_path}}: {conditionMessage(e)}"
      )
      list(
        source_path = abs_path,
        registered = NULL,
        size = NULL,
        sha256 = NULL,
        skipped = FALSE,
        same_file = same_file,
        error = conditionMessage(e)
      )
    }
  )
}

#' Fetch a fresh copy of a remote file to a temp path for comparison
#'
#' Used by `att_refresh()` for remote sources. Downloads to `tmp_file` and
#' returns a comparison record.
#' @noRd
refresh_fetch_remote <- function(fname, file_info, tmp_file) {
  url <- file_info$url
  recorded_hash <- file_info$sha256 %||% NA_character_
  recorded_size <- as.numeric(file_info$size %||% NA_real_)

  cli::cli_alert("Fetching {.file {fname}}")

  new_hash <- tryCatch(
    {
      resp <- att_request(url) |> httr2::req_perform()
      writeBin(httr2::resp_body_raw(resp), tmp_file)
      att_hash(tmp_file)
    },
    error = function(e) {
      cli::cli_alert_danger(
        "Failed to fetch {.file {fname}}: {conditionMessage(e)}"
      )
      NA_character_
    }
  )

  new_size <- if (file.exists(tmp_file)) file.size(tmp_file) else NA_real_

  status <- if (is.na(new_hash)) {
    "error"
  } else if (identical(recorded_hash, new_hash)) {
    "unchanged"
  } else {
    "changed"
  }

  list(
    file = fname,
    status = status,
    old_hash = recorded_hash,
    new_hash = new_hash,
    old_size = recorded_size,
    new_size = new_size,
    location = file_info$location %||% "root",
    tmp_file = tmp_file
  )
}

#' Fetch a fresh copy of a local file to a temp path for comparison
#'
#' Used by `att_refresh()` for local sources. Copies from the original
#' `source_path` to `tmp_file` and returns a comparison record.
#' @noRd
refresh_fetch_local <- function(fname, file_info, tmp_file) {
  source_path <- file_info$source_path
  recorded_hash <- file_info$sha256 %||% NA_character_
  recorded_size <- as.numeric(file_info$size %||% NA_real_)

  cli::cli_alert("Checking {.file {fname}} from {.path {source_path}}")

  new_hash <- tryCatch(
    {
      if (!file.exists(source_path)) {
        cli::cli_alert_danger(
          "Source file not found: {.path {source_path}}"
        )
        NA_character_
      } else {
        file.copy(source_path, tmp_file, overwrite = TRUE)
        att_hash(tmp_file)
      }
    },
    error = function(e) {
      cli::cli_alert_danger(
        "Failed to read {.file {fname}}: {conditionMessage(e)}"
      )
      NA_character_
    }
  )

  new_size <- if (file.exists(tmp_file)) file.size(tmp_file) else NA_real_

  status <- if (is.na(new_hash)) {
    "error"
  } else if (identical(recorded_hash, new_hash)) {
    "unchanged"
  } else {
    "changed"
  }

  list(
    file = fname,
    status = status,
    old_hash = recorded_hash,
    new_hash = new_hash,
    old_size = recorded_size,
    new_size = new_size,
    location = file_info$location %||% "root",
    tmp_file = tmp_file
  )
}

#' Fetch a remote file to a temp path for comparison, returning just the hash
#'
#' Used by `att_compare()` for remote sources.
#' @noRd
compare_fetch_remote <- function(fname, file_info, tmp) {
  url <- file_info$url
  cli::cli_alert("Fetching {.file {fname}}")
  tryCatch(
    {
      resp <- att_request(url) |> httr2::req_perform()
      writeBin(httr2::resp_body_raw(resp), tmp)
      att_hash(tmp)
    },
    error = function(e) {
      cli::cli_alert_danger(
        "Failed to fetch {.file {fname}}: {conditionMessage(e)}"
      )
      NA_character_
    }
  )
}

#' Read a local source file to a temp path for comparison, returning just the hash
#'
#' Used by `att_compare()` for local sources. Hashes the file at its original
#' `source_path` directly (no copy needed for read-only comparison).
#' @noRd
compare_fetch_local <- function(fname, file_info, tmp) {
  source_path <- file_info$source_path
  cli::cli_alert("Checking {.file {fname}} from {.path {source_path}}")
  tryCatch(
    {
      if (!file.exists(source_path)) {
        cli::cli_alert_danger(
          "Source file not found: {.path {source_path}}"
        )
        return(NA_character_)
      }
      att_hash(source_path)
    },
    error = function(e) {
      cli::cli_alert_danger(
        "Failed to read {.file {fname}}: {conditionMessage(e)}"
      )
      NA_character_
    }
  )
}

#' Derive filenames from local paths, using names if provided
#' @noRd
derive_local_filename <- function(paths, index) {
  nm <- names(paths)[index]
  if (!is.null(nm) && nchar(nm) > 0) {
    return(nm)
  }
  basename(paths[index])
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
