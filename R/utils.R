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

#' Build a base httr2 request with user-agent
#' @noRd
att_request <- function(url) {
  httr2::request(url) |>
    httr2::req_user_agent("attest (R package; data provenance tracking)") |>
    # Force HTTP/1.1 to avoid HTTP/2 framing errors with libcurl + Schannel
    # on Windows (affects api.gbif.org, www.canada.ca, and others).
    httr2::req_options(http_version = 2L)
}


#' Stream a URL to a file with a live CLI progress bar.
#'
#' Uses req_perform_connection() so chunks are read in R, allowing the
#' progress bar to update in real time (unlike req_progress() / req_perform()
#' which only flush at the end in RStudio).
#'
#' Returns the httr2 response object (headers still accessible).
#' @noRd
stream_download <- function(url, dest) {
  bytes <- 0
  size_str <- "0.0 MB"
  start <- proc.time()[["elapsed"]]

  bar <- cli::cli_progress_bar(
    total = NA,
    format = "{cli::pb_spin} {size_str} downloaded",
    clear = TRUE,
    .auto_close = FALSE,
    .envir = environment()
  )

  con <- file(dest, "wb")
  on.exit(try(close(con), silent = TRUE), add = TRUE)

  resp <- att_request(url) |> httr2::req_perform_connection()
  on.exit({
    elapsed <- round(proc.time()[["elapsed"]] - start)
    cli::cli_progress_done(id = bar)
    cli::cli_alert_success("Downloaded {size_str} in {elapsed}s")
  }, add = TRUE)

  while (!httr2::resp_stream_is_complete(resp)) {
    chunk <- httr2::resp_stream_raw(resp, kb = 256)
    if (length(chunk) == 0L) break
    writeBin(chunk, con)
    bytes <- bytes + as.numeric(length(chunk))
    size_str <- if (bytes >= 1024^3) {
      paste(format(round(bytes / 1024^3, 2), nsmall = 2), "GB")
    } else {
      paste(format(round(bytes / 1024^2, 1), nsmall = 1), "MB")
    }
    cli::cli_progress_update(id = bar, .envir = environment())
  }

  resp
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
      resp <- stream_download(url, dest)

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
  file.path(store, name, "_attest", "provenance.json")
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
        source_modified = format(file.mtime(abs_path), "%Y-%m-%dT%H:%M:%S%z"),
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
      att_request(url) |> httr2::req_perform(path = tmp_file)
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
      att_request(url) |> httr2::req_perform(path = tmp)
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


#' Parse our own BibTeX file into a list of entry lists
#'
#' Only handles the predictable format produced by `build_bib_entry()`.
#' @noRd
parse_bib_file <- function(bib_path) {
  if (!file.exists(bib_path)) return(list())
  lines <- readLines(bib_path, warn = FALSE)
  text <- paste(lines, collapse = "\n")

  # Split into entries
  entry_pattern <- "@\\w+\\{[^,]+,(?s).*?\\n\\}"
  matches <- gregexpr(entry_pattern, text, perl = TRUE)
  entries_raw <- regmatches(text, matches)[[1]]

  lapply(entries_raw, function(entry) {
    # Extract key
    key <- sub("^@\\w+\\{([^,]+),.*", "\\1", entry)

    # Extract fields: "  name = {value}"
    field_pattern <- "^\\s+(\\w+)\\s*=\\s*\\{(.*)\\}"
    field_lines <- strsplit(entry, "\n")[[1]]
    fields <- list()
    for (line in field_lines) {
      m <- regmatches(line, regexec(field_pattern, line))[[1]]
      if (length(m) == 3) {
        # Strip outer braces from author field ({...})
        val <- m[3]
        val <- sub("^\\{(.*)\\}$", "\\1", val)
        fields[[m[2]]] <- val
      }
    }

    fields$key <- key
    fields
  })
}


#' Format a parsed BibTeX entry as an APA-style markdown citation
#' @noRd
format_markdown_citation <- function(entry) {
  author <- entry$author %||% "Unknown"
  year <- entry$year %||% "n.d."
  title <- entry$title %||% entry$key
  note <- entry$note
  url <- entry$url

  # Author. (Year). *Title* [Note]. URL
  parts <- paste0(author, ". (", year, "). *", title, "*")
  if (!is.null(note)) parts <- paste0(parts, " ", note)
  parts <- paste0(parts, ".")
  if (!is.null(url)) parts <- paste0(parts, " ", url)

  parts
}


#' Regenerate data-sources.md from data-sources.bib
#' @noRd
sync_markdown_citations <- function(bib_path) {
  md_path <- sub("\\.bib$", ".md", bib_path)

  entries <- parse_bib_file(bib_path)

  if (length(entries) == 0) {
    if (file.exists(md_path)) file.remove(md_path)
    return(invisible(NULL))
  }

  # Sort by author then year
  sort_keys <- vapply(entries, function(e) {
    paste0(tolower(e$author %||% ""), "|", e$year %||% "")
  }, character(1))
  entries <- entries[order(sort_keys)]

  citations <- vapply(entries, format_markdown_citation, character(1))
  content <- c("# Data Sources", "", citations, "")

  writeLines(content, md_path)
  invisible(md_path)
}
