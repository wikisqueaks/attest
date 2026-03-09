# Archive handling ----------------------------------------------------------

#' Check whether a URL points to a zip archive
#' @noRd
is_archive_url <- function(url) {
  clean_url <- sub("[?#].*$", "", url)
  grepl("\\.zip$", clean_url, ignore.case = TRUE)
}

#' Suggest a default role for an extracted file based on extension
#' @noRd
suggest_file_role <- function(filename) {
  base <- tolower(basename(filename))

  # Only PDFs default to metadata — they're almost always documentation
  if (tolower(tools::file_ext(filename)) == "pdf") return("metadata")

  # Name-based patterns for documentation files regardless of extension
  metadata_patterns <- c("readme", "codebook", "dictionary", "metadata",
                         "license", "changelog")
  for (pat in metadata_patterns) {
    if (grepl(pat, base, ignore.case = TRUE)) return("metadata")
  }

  "data"
}


#' Interactively classify extracted files
#'
#' Presents extracted file names and suggested roles, allowing the user to
#' accept defaults or reclassify in bulk.
#'
#' @param files Character vector of file names.
#' @param classify Named list with `data` and/or `metadata` and/or `ignore`
#'   elements containing file extensions (e.g.,
#'   `list(data = c(".shp", ".dbf"), metadata = ".pdf")`).
#'   Used in non-interactive sessions. `NULL` for interactive prompt.
#' @return A named character vector: names are file names, values are roles
#'   (`"data"`, `"metadata"`, or `"ignore"`).
#' @noRd
classify_extracted_files <- function(files, classify = NULL) {
  roles <- vapply(files, suggest_file_role, character(1), USE.NAMES = TRUE)

  if (!is.null(classify)) {
    return(classify_by_extension(files, classify))
  }

  if (!interactive()) {
    cli::cli_alert_info(
      "Non-interactive session: all extracted files classified as data."
    )
    roles[] <- "data"
    return(roles)
  }

  role_labels <- c(data = "d", metadata = "m", ignore = "i")
  display_classification(files, roles, role_labels)

  repeat {
    input <- readline(
      "Press ENTER to accept, or reclassify (e.g., '5,6 m' or '1-4 i' or 'all d'): "
    )
    input <- trimws(input)

    if (nchar(input) == 0) break

    parsed <- parse_classify_input(input, length(files))
    if (is.null(parsed)) {
      cli::cli_alert_warning(
        "Could not parse input. Examples: '1,3,5 d', '2-4 m', '6 i', 'all d'"
      )
      next
    }

    roles[parsed$indices] <- parsed$role
    display_classification(files, roles, role_labels)
  }

  roles
}


#' Display file classification table
#' @noRd
display_classification <- function(files, roles, role_labels) {
  cli::cli_text("")
  cli::cli_text(
    "Classify files: {.strong [d]}ata, {.strong [m]}etadata, {.strong [i]}gnore"
  )
  for (i in seq_along(files)) {
    label <- role_labels[roles[i]]
    cli::cli_text("  [{label}] {i}. {files[i]}")
  }
  cli::cli_text("")
}


#' Parse classification input like "1,3,5 d" or "2-4 m" or "all d"
#' @noRd
parse_classify_input <- function(input, n_files) {
  parts <- strsplit(trimws(input), "\\s+")[[1]]
  if (length(parts) != 2) return(NULL)

  role_map <- c(d = "data", m = "metadata", i = "ignore")
  role <- role_map[tolower(parts[2])]
  if (is.na(role)) return(NULL)

  if (tolower(parts[1]) == "all") {
    return(list(indices = seq_len(n_files), role = unname(role)))
  }

  indices <- parse_indices(parts[1], n_files)
  if (is.null(indices)) return(NULL)

  list(indices = indices, role = unname(role))
}


#' Parse index specification like "1,3,5" or "2-4" or "1-3,5,7-9"
#' @noRd
parse_indices <- function(spec, max_n) {
  parts <- strsplit(spec, ",")[[1]]
  indices <- integer(0)

  for (part in parts) {
    part <- trimws(part)
    if (grepl("^\\d+-\\d+$", part)) {
      range_vals <- as.integer(strsplit(part, "-")[[1]])
      if (any(is.na(range_vals)) || range_vals[1] > range_vals[2]) return(NULL)
      indices <- c(indices, seq(range_vals[1], range_vals[2]))
    } else if (grepl("^\\d+$", part)) {
      indices <- c(indices, as.integer(part))
    } else {
      return(NULL)
    }
  }

  indices <- unique(indices)
  if (length(indices) == 0 || any(indices < 1 | indices > max_n)) return(NULL)
  indices
}


#' Classify files by extension mapping (non-interactive fallback)
#' @noRd
classify_by_extension <- function(files, classify) {
  roles <- rep("data", length(files))
  names(roles) <- files

  normalize_ext <- function(exts) {
    ifelse(startsWith(exts, "."), exts, paste0(".", exts))
  }

  file_exts <- paste0(".", tolower(tools::file_ext(files)))

  if (!is.null(classify$metadata)) {
    meta_exts <- normalize_ext(classify$metadata)
    roles[file_exts %in% meta_exts] <- "metadata"
  }

  if (!is.null(classify$ignore)) {
    ign_exts <- normalize_ext(classify$ignore)
    roles[file_exts %in% ign_exts] <- "ignore"
  }

  # Explicit data overrides (in case of overlap)
  if (!is.null(classify$data)) {
    data_exts <- normalize_ext(classify$data)
    roles[file_exts %in% data_exts] <- "data"
  }

  roles
}


#' Download, extract, classify, and place files from an archive URL
#'
#' @param url URL of the zip archive.
#' @param archive_name Filename for the archive (used as key in provenance).
#' @param source_dir Path to the source directory (for data files).
#' @param metadata_dir Path to the metadata subdirectory.
#' @param classify Extension-based classification (for non-interactive use).
#' @return A list with `archive` (record for the archive itself), `files`
#'   (named list of file records), and `failures` (character vector of failed
#'   file names).
#' @noRd
process_archive_url <- function(url, archive_name, source_dir, metadata_dir,
                                classify = NULL) {
  cli::cli_alert("Downloading archive {.url {url}}")

  tmp_zip <- tempfile(fileext = ".zip")
  on.exit(unlink(tmp_zip), add = TRUE)

  archive_record <- tryCatch(
    {
      resp <- att_request(url) |> httr2::req_perform()
      writeBin(httr2::resp_body_raw(resp), tmp_zip)

      list(
        url = url,
        downloaded = timestamp_now(),
        sha256 = att_hash(tmp_zip),
        size = file.size(tmp_zip),
        http_etag = httr2::resp_header(resp, "ETag"),
        http_last_modified = httr2::resp_header(resp, "Last-Modified"),
        http_content_type = httr2::resp_header(resp, "Content-Type"),
        error = NULL
      )
    },
    error = function(e) {
      cli::cli_alert_danger(
        "Failed to download archive: {conditionMessage(e)}"
      )
      list(url = url, downloaded = NULL, sha256 = NULL, size = NULL,
           error = conditionMessage(e))
    }
  )

  if (!is.null(archive_record$error)) {
    return(list(
      archive = archive_record,
      files = list(),
      failures = archive_name
    ))
  }

  # Extract
  tmp_dir <- tempfile("attest_extract_")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  extracted <- tryCatch(
    utils::unzip(tmp_zip, exdir = tmp_dir),
    error = function(e) {
      cli::cli_alert_danger(
        "Failed to extract archive: {conditionMessage(e)}"
      )
      NULL
    }
  )

  if (is.null(extracted)) {
    archive_record$error <- "Extraction failed"
    return(list(
      archive = archive_record,
      files = list(),
      failures = archive_name
    ))
  }

  # List files (recursive, relative paths)
  rel_paths <- list.files(tmp_dir, recursive = TRUE)

  if (length(rel_paths) == 0) {
    cli::cli_alert_warning("Archive was empty.")
    return(list(
      archive = archive_record,
      files = list(),
      failures = character(0)
    ))
  }

  # Use basenames for placement; check for collisions
  basenames <- basename(rel_paths)
  if (anyDuplicated(basenames)) {
    cli::cli_alert_warning(
      "Archive contains files with duplicate names in different directories. ",
      "Using full relative paths."
    )
    basenames <- rel_paths
  }

  cli::cli_alert_success(
    "Extracted {length(rel_paths)} file{?s} from {.file {archive_name}}"
  )

  # Classify
  roles <- classify_extracted_files(basenames, classify = classify)

  # Place files and record provenance
  files_record <- list()
  failures <- character(0)

  for (j in seq_along(rel_paths)) {
    fname <- basenames[j]
    role <- unname(roles[j])

    if (role == "ignore") next

    src_path <- file.path(tmp_dir, rel_paths[j])

    if (role == "metadata") {
      dest <- file.path(metadata_dir, fname)
      location <- "metadata"
    } else {
      dest <- file.path(source_dir, fname)
      location <- "root"
    }

    tryCatch(
      {
        file.copy(src_path, dest, overwrite = FALSE)
        files_record[[fname]] <- list(
          extracted_from = archive_name,
          size = file.size(dest),
          sha256 = att_hash(dest),
          location = location,
          error = NULL
        )
      },
      error = function(e) {
        cli::cli_alert_danger(
          "Failed to place {.file {fname}}: {conditionMessage(e)}"
        )
        files_record[[fname]] <<- list(
          extracted_from = archive_name,
          size = NULL,
          sha256 = NULL,
          location = location,
          error = conditionMessage(e)
        )
        failures <<- c(failures, fname)
      }
    )
  }

  list(archive = archive_record, files = files_record, failures = failures)
}


# Helpers for compare / refresh / check on archive sources ------------------

#' Re-download an archive and compare its hash (read-only)
#'
#' Used by `att_compare()`. Downloads the archive to a temp file, hashes it,
#' and reports whether it matches the recorded hash. Does not extract.
#' @noRd
compare_fetch_archive <- function(archive_name, archive_info) {
  url <- archive_info$url
  recorded_hash <- archive_info$sha256 %||% NA_character_

  cli::cli_alert("Fetching archive {.file {archive_name}}")

  tmp_zip <- tempfile(fileext = ".zip")
  on.exit(unlink(tmp_zip), add = TRUE)

  new_hash <- tryCatch(
    {
      resp <- att_request(url) |> httr2::req_perform()
      writeBin(httr2::resp_body_raw(resp), tmp_zip)
      att_hash(tmp_zip)
    },
    error = function(e) {
      cli::cli_alert_danger(
        "Failed to fetch archive {.file {archive_name}}: {conditionMessage(e)}"
      )
      NA_character_
    }
  )

  status <- if (is.na(new_hash)) {
    "error"
  } else if (identical(recorded_hash, new_hash)) {
    "match"
  } else {
    "changed"
  }

  list(
    archive = archive_name,
    status = status,
    recorded_hash = recorded_hash,
    source_hash = new_hash
  )
}


#' Re-download an archive for refresh, extract and compare individual files
#'
#' Used by `att_refresh()`. Downloads the archive, compares its hash, and if
#' changed, extracts and returns per-file comparison records compatible with
#' the regular refresh flow.
#'
#' @param archive_name Filename key in `prov$archives`.
#' @param archive_info The archive record from provenance.
#' @param file_records Named list of file records (subset of `prov$files`)
#'   that were extracted from this archive.
#' @param tmp_dir Temporary directory for downloads and extraction.
#' @return A list with `archive_hash`, `archive_changed`, `http_headers`,
#'   and `files` (list of per-file comparison records).
#' @noRd
refresh_fetch_archive <- function(archive_name, archive_info, file_records,
                                  tmp_dir) {
  url <- archive_info$url
  recorded_hash <- archive_info$sha256 %||% NA_character_

  cli::cli_alert("Fetching archive {.file {archive_name}}")

  tmp_zip <- file.path(tmp_dir, archive_name)

  download_result <- tryCatch(
    {
      resp <- att_request(url) |> httr2::req_perform()
      writeBin(httr2::resp_body_raw(resp), tmp_zip)
      list(
        hash = att_hash(tmp_zip),
        http_etag = httr2::resp_header(resp, "ETag"),
        http_last_modified = httr2::resp_header(resp, "Last-Modified"),
        http_content_type = httr2::resp_header(resp, "Content-Type")
      )
    },
    error = function(e) {
      cli::cli_alert_danger(
        "Failed to fetch archive {.file {archive_name}}: {conditionMessage(e)}"
      )
      NULL
    }
  )

  # Download failed — mark all files as error
  if (is.null(download_result)) {
    error_entries <- lapply(names(file_records), function(fname) {
      list(
        file = fname,
        status = "error",
        old_hash = file_records[[fname]]$sha256 %||% NA_character_,
        new_hash = NA_character_,
        old_size = as.numeric(file_records[[fname]]$size %||% NA_real_),
        new_size = NA_real_,
        location = file_records[[fname]]$location %||% "root",
        tmp_file = NA_character_
      )
    })
    return(list(
      archive_hash = NA_character_,
      archive_changed = NA,
      http_headers = NULL,
      files = error_entries
    ))
  }

  new_hash <- download_result$hash
  archive_changed <- !identical(recorded_hash, new_hash)

  # Archive unchanged — all files unchanged
  if (!archive_changed) {
    unchanged_entries <- lapply(names(file_records), function(fname) {
      list(
        file = fname,
        status = "unchanged",
        old_hash = file_records[[fname]]$sha256 %||% NA_character_,
        new_hash = file_records[[fname]]$sha256 %||% NA_character_,
        old_size = as.numeric(file_records[[fname]]$size %||% NA_real_),
        new_size = as.numeric(file_records[[fname]]$size %||% NA_real_),
        location = file_records[[fname]]$location %||% "root",
        tmp_file = NA_character_
      )
    })
    return(list(
      archive_hash = new_hash,
      archive_changed = FALSE,
      http_headers = download_result[c("http_etag", "http_last_modified",
                                       "http_content_type")],
      files = unchanged_entries
    ))
  }

  # Archive changed — extract and compare individual files
  extract_dir <- file.path(tmp_dir, "extracted")
  dir.create(extract_dir, showWarnings = FALSE)

  tryCatch(
    utils::unzip(tmp_zip, exdir = extract_dir),
    error = function(e) {
      cli::cli_alert_danger(
        "Failed to extract archive: {conditionMessage(e)}"
      )
    }
  )

  extracted_files <- list.files(extract_dir, recursive = TRUE)
  extracted_basenames <- basename(extracted_files)

  file_comparisons <- lapply(names(file_records), function(fname) {
    file_info <- file_records[[fname]]
    old_hash <- file_info$sha256 %||% NA_character_
    old_size <- as.numeric(file_info$size %||% NA_real_)

    match_idx <- which(extracted_basenames == fname)

    if (length(match_idx) == 0) {
      cli::cli_alert_warning(
        "{.file {fname}}: no longer present in archive"
      )
      return(list(
        file = fname, status = "error",
        old_hash = old_hash, new_hash = NA_character_,
        old_size = old_size, new_size = NA_real_,
        location = file_info$location %||% "root",
        tmp_file = NA_character_
      ))
    }

    tmp_file <- file.path(extract_dir, extracted_files[match_idx[1]])
    new_hash <- att_hash(tmp_file)
    new_size <- file.size(tmp_file)
    status <- if (identical(old_hash, new_hash)) "unchanged" else "changed"

    list(
      file = fname, status = status,
      old_hash = old_hash, new_hash = new_hash,
      old_size = old_size, new_size = new_size,
      location = file_info$location %||% "root",
      tmp_file = tmp_file
    )
  })

  list(
    archive_hash = new_hash,
    archive_changed = TRUE,
    http_headers = download_result[c("http_etag", "http_last_modified",
                                     "http_content_type")],
    files = file_comparisons
  )
}


#' HTTP HEAD check on an archive URL
#'
#' Used by `att_check()` for archive sources. Compares ETag, Last-Modified,
#' and Content-Length headers.
#' @noRd
check_archive_head <- function(archive_name, archive_info) {
  url <- archive_info$url

  cli::cli_alert("Checking archive {.url {url}}")

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
    return(list(status = "unreachable", url = url))
  }

  remote_etag <- httr2::resp_header(resp, "ETag")
  remote_modified <- httr2::resp_header(resp, "Last-Modified")
  remote_length <- httr2::resp_header(resp, "Content-Length")

  changes <- character(0)
  compared <- FALSE

  if (!is.null(remote_etag) && !is.null(archive_info$http_etag)) {
    compared <- TRUE
    if (remote_etag != archive_info$http_etag) {
      changes <- c(changes, "ETag changed")
    }
  }

  if (!is.null(remote_modified) && !is.null(archive_info$http_last_modified)) {
    compared <- TRUE
    if (remote_modified != archive_info$http_last_modified) {
      changes <- c(changes, "Last-Modified changed")
    }
  }

  if (!is.null(remote_length)) {
    compared <- TRUE
    recorded_size <- archive_info$size %||% 0
    if (as.numeric(remote_length) != recorded_size) {
      changes <- c(changes, "Content-Length changed")
    }
  }

  if (length(changes) > 0) {
    cli::cli_alert_warning(
      "Archive {.file {archive_name}}: {paste(changes, collapse = ', ')}"
    )
    list(status = "possibly_changed", url = url, changes = changes)
  } else if (!compared) {
    cli::cli_alert_info(
      "Archive {.file {archive_name}}: No headers available for comparison"
    )
    list(status = "no_comparison", url = url)
  } else {
    cli::cli_alert_success(
      "Archive {.file {archive_name}}: No changes detected"
    )
    list(status = "unchanged", url = url)
  }
}
