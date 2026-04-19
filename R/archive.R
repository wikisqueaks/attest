# Archive handling ----------------------------------------------------------

#' Check whether a URL points to a supported archive format
#' @noRd
is_archive_url <- function(url) {
  clean_url <- sub("[?#].*$", "", url)
  grepl("\\.(zip|tar\\.gz|tgz)$", clean_url, ignore.case = TRUE)
}

#' Check whether a local path is a supported archive format
#' @noRd
is_archive_path <- function(path) {
  !is.na(archive_type(path))
}

#' Detect archive type from a filename or URL
#' @return `"zip"`, `"tar.gz"`, or `NA_character_` if not recognized.
#' @noRd
archive_type <- function(path) {
  clean <- sub("[?#].*$", "", path)
  if (grepl("\\.zip$", clean, ignore.case = TRUE)) return("zip")
  if (grepl("\\.(tar\\.gz|tgz)$", clean, ignore.case = TRUE)) return("tar.gz")
  NA_character_
}

#' Extract an archive to a directory
#'
#' Dispatches to `utils::unzip()` or `utils::untar()` based on archive type.
#' @param archive_path Path to the archive file.
#' @param exdir Directory to extract into.
#' @param type Archive type: `"zip"` or `"tar.gz"`.
#' @return Character vector of extracted file paths (from `unzip`/`untar`),
#'   or `NULL` on failure.
#' @noRd
extract_archive <- function(archive_path, exdir, type) {
  tryCatch(
    {
      if (type == "zip") {
        utils::unzip(archive_path, exdir = exdir)
      } else {
        utils::untar(archive_path, exdir = exdir)
      }
    },
    error = function(e) {
      cli::cli_alert_danger(
        "Failed to extract archive: {conditionMessage(e)}"
      )
      NULL
    }
  )
}

#' Check whether a file extension is unambiguous enough to auto-classify
#'
#' Returns TRUE for extensions where the role (data or metadata) is
#' well-known, meaning we can skip the interactive classification prompt.
#' @noRd
is_known_extension <- function(filename) {
  ext <- tolower(tools::file_ext(filename))
  base <- tolower(basename(filename))

  known_data <- c(
    # Shapefile components
    "shp", "dbf", "shx", "prj", "cpg", "sbn", "sbx",
    "fbn", "fbx", "ain", "aih", "atx", "ixs", "mxs",
    # Tabular
    "csv", "tsv", "parquet", "feather", "arrow", "xlsx", "xls",
    # Geospatial
    "geojson", "gpkg", "kml", "gml", "tif", "tiff", "gpx",
    # Other common data
    "json", "rds", "rda", "rdata", "sav", "dta", "sqlite"
    # Note: .gdb (File Geodatabase) is a directory format, not a file —
    # it is handled separately by find_dir_format_dirs()
  )

  known_metadata <- "pdf"

  if (ext %in% c(known_data, known_metadata)) return(TRUE)

  # Shapefile XML sidecar (.shp.xml)
  if (ext == "xml" && grepl("\\.shp\\.xml$", base)) return(TRUE)

  # Documentation name patterns are also confident
  metadata_patterns <- c("readme", "codebook", "dictionary", "metadata",
                         "license", "changelog")
  for (pat in metadata_patterns) {
    if (grepl(pat, base, ignore.case = TRUE)) return(TRUE)
  }

  FALSE
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

  # Skip prompt when all files have well-known extensions
  all_known <- all(vapply(files, is_known_extension, logical(1)))
  if (all_known) {
    cli::cli_alert_info("All files have known extensions; auto-classifying.")
    return(roles)
  }

  if (!rlang::is_interactive()) {
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


#' Find directory-format data containers after archive extraction
#'
#' Some geospatial formats are directories, not files (e.g., ESRI File
#' Geodatabases). When present inside a zip, `list.files(recursive = TRUE)`
#' would descend into them and produce hundreds of opaque internal files.
#' This function identifies those directories so they can be treated as
#' atomic units — copied whole and recorded as a single provenance entry.
#' @param exdir Path to the extraction directory.
#' @return Character vector of relative paths (from `exdir`) to
#'   directory-format containers, e.g. `"mydata.gdb"`.
#' @noRd
find_dir_format_dirs <- function(exdir) {
  dir_format_exts <- c("gdb")
  all_dirs <- list.dirs(exdir, recursive = TRUE, full.names = FALSE)
  all_dirs <- all_dirs[nchar(all_dirs) > 0]
  all_dirs[tolower(tools::file_ext(basename(all_dirs))) %in% dir_format_exts]
}


#' Download, extract, classify, and place files from an archive URL
#'
#' @param url URL of the archive (`.zip`, `.tar.gz`, or `.tgz`).
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

  type <- archive_type(url)
  ext <- if (type == "zip") ".zip" else ".tar.gz"
  tmp_archive <- tempfile(fileext = ext)
  on.exit(unlink(tmp_archive), add = TRUE)

  archive_record <- tryCatch(
    {
      resp <- stream_download(url, tmp_archive)

      list(
        url = url,
        downloaded = timestamp_now(),
        sha256 = att_hash(tmp_archive),
        size = file.size(tmp_archive),
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

  extracted <- extract_archive(tmp_archive, tmp_dir, type)

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

  # Detect directory-format containers (e.g., .gdb) and exclude their
  # contents from per-file classification — they'll be placed as a unit
  dir_fmt_dirs <- find_dir_format_dirs(tmp_dir)
  if (length(dir_fmt_dirs) > 0) {
    in_dir_fmt <- vapply(rel_paths, function(p) {
      any(startsWith(p, paste0(dir_fmt_dirs, "/")))
    }, logical(1))
    rel_paths <- rel_paths[!in_dir_fmt]
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

  n_total <- length(rel_paths) + length(dir_fmt_dirs)
  cli::cli_alert_success(
    "Extracted {n_total} item{?s} from {.file {archive_name}}"
  )

  # Classify and place regular files
  roles <- classify_extracted_files(basenames, classify = classify)

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

  # Place directory-format containers as atomic units (not hashed)
  for (dir_rel in dir_fmt_dirs) {
    dir_name <- basename(dir_rel)
    src_dir <- file.path(tmp_dir, dir_rel)
    tryCatch(
      {
        file.copy(src_dir, source_dir, recursive = TRUE)
        cli::cli_alert_info(
          "{.file {dir_name}}: placed as directory unit (not hashed)"
        )
        files_record[[dir_name]] <- list(
          extracted_from = archive_name,
          type = "directory",
          location = "root",
          error = NULL
        )
      },
      error = function(e) {
        cli::cli_alert_danger(
          "Failed to place {.file {dir_name}}: {conditionMessage(e)}"
        )
        files_record[[dir_name]] <<- list(
          extracted_from = archive_name,
          type = "directory",
          location = "root",
          error = conditionMessage(e)
        )
        failures <<- c(failures, dir_name)
      }
    )
  }

  list(archive = archive_record, files = files_record, failures = failures)
}


#' Extract, classify, and place files from a local archive
#'
#' The local-file counterpart to `process_archive_url()`. Hashes the archive,
#' extracts to a temp directory, classifies files, and places them in the
#' source/metadata directories.
#'
#' @param path Path to the local archive file.
#' @param archive_name Filename for the archive (used as key in provenance).
#' @param source_dir Path to the source directory (for data files).
#' @param metadata_dir Path to the metadata subdirectory.
#' @param classify Extension-based classification (for non-interactive use).
#' @return A list with `archive` (record for the archive itself), `files`
#'   (named list of file records), and `failures` (character vector of failed
#'   file names).
#' @noRd
process_archive_path <- function(path, archive_name, source_dir, metadata_dir,
                                 classify = NULL) {
  abs_path <- normalizePath(path, mustWork = FALSE)
  cli::cli_alert("Registering archive {.path {abs_path}}")

  if (!file.exists(abs_path)) {
    cli::cli_alert_danger("Archive not found: {.path {abs_path}}")
    return(list(
      archive = list(
        source_path = abs_path, registered = NULL, sha256 = NULL,
        size = NULL, error = paste0("File not found: ", abs_path)
      ),
      files = list(),
      failures = archive_name
    ))
  }

  type <- archive_type(path)
  archive_record <- list(
    source_path = abs_path,
    registered = timestamp_now(),
    sha256 = att_hash(abs_path),
    size = file.size(abs_path),
    source_modified = format(file.mtime(abs_path), "%Y-%m-%dT%H:%M:%S%z"),
    error = NULL
  )

  # Extract
  tmp_dir <- tempfile("attest_extract_")
  dir.create(tmp_dir)
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)

  extracted <- extract_archive(abs_path, tmp_dir, type)

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

  # Detect directory-format containers (e.g., .gdb) and exclude their
  # contents from per-file classification — they'll be placed as a unit
  dir_fmt_dirs <- find_dir_format_dirs(tmp_dir)
  if (length(dir_fmt_dirs) > 0) {
    in_dir_fmt <- vapply(rel_paths, function(p) {
      any(startsWith(p, paste0(dir_fmt_dirs, "/")))
    }, logical(1))
    rel_paths <- rel_paths[!in_dir_fmt]
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

  n_total <- length(rel_paths) + length(dir_fmt_dirs)
  cli::cli_alert_success(
    "Extracted {n_total} item{?s} from {.file {archive_name}}"
  )

  # Classify and place regular files
  roles <- classify_extracted_files(basenames, classify = classify)

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

  # Place directory-format containers as atomic units (not hashed)
  for (dir_rel in dir_fmt_dirs) {
    dir_name <- basename(dir_rel)
    src_dir <- file.path(tmp_dir, dir_rel)
    tryCatch(
      {
        file.copy(src_dir, source_dir, recursive = TRUE)
        cli::cli_alert_info(
          "{.file {dir_name}}: placed as directory unit (not hashed)"
        )
        files_record[[dir_name]] <- list(
          extracted_from = archive_name,
          type = "directory",
          location = "root",
          error = NULL
        )
      },
      error = function(e) {
        cli::cli_alert_danger(
          "Failed to place {.file {dir_name}}: {conditionMessage(e)}"
        )
        files_record[[dir_name]] <<- list(
          extracted_from = archive_name,
          type = "directory",
          location = "root",
          error = conditionMessage(e)
        )
        failures <<- c(failures, dir_name)
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

  type <- archive_type(archive_info$url)
  ext <- if (identical(type, "zip")) ".zip" else ".tar.gz"
  tmp_archive <- tempfile(fileext = ext)
  on.exit(unlink(tmp_archive), add = TRUE)

  new_hash <- tryCatch(
    {
      stream_download(url, tmp_archive)
      att_hash(tmp_archive)
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

  tmp_archive <- file.path(tmp_dir, archive_name)

  download_result <- tryCatch(
    {
      resp <- stream_download(url, tmp_archive)
      list(
        hash = att_hash(tmp_archive),
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

  type <- archive_type(url)
  extract_archive(tmp_archive, extract_dir, type)

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


# Local archive helpers ----------------------------------------------------

#' Stat-based check on a local archive file
#'
#' Used by `att_check()` for local archive sources. Compares file size and
#' modification time against the recorded provenance values.
#' @noRd
check_archive_local <- function(archive_name, archive_info) {
  source_path <- archive_info$source_path

  cli::cli_alert("Checking archive {.path {source_path}}")

  if (is.null(source_path) || !file.exists(source_path)) {
    cli::cli_alert_danger(
      "Archive {.file {archive_name}}: source file no longer exists"
    )
    return(list(status = "source_missing", source_path = source_path))
  }

  changes <- character(0)
  recorded_size <- archive_info$size %||% 0

  current_size <- file.size(source_path)
  if (current_size != recorded_size) {
    changes <- c(changes, "size changed")
  }

  if (!is.null(archive_info$source_modified)) {
    current_mtime <- format(
      file.mtime(source_path), "%Y-%m-%dT%H:%M:%S%z"
    )
    if (current_mtime != archive_info$source_modified) {
      changes <- c(changes, "modified time changed")
    }
  }

  if (length(changes) > 0) {
    cli::cli_alert_warning(
      "Archive {.file {archive_name}}: {paste(changes, collapse = ', ')}"
    )
    list(
      status = "possibly_changed", source_path = source_path,
      changes = changes
    )
  } else {
    cli::cli_alert_success(
      "Archive {.file {archive_name}}: No changes detected"
    )
    list(status = "unchanged", source_path = source_path)
  }
}


#' Hash-based comparison of a local archive (read-only)
#'
#' Used by `att_compare()` for local archive sources. Hashes the archive at
#' its `source_path` and compares to the recorded hash.
#' @noRd
compare_fetch_local_archive <- function(archive_name, archive_info) {
  source_path <- archive_info$source_path
  recorded_hash <- archive_info$sha256 %||% NA_character_

  cli::cli_alert("Checking archive {.file {archive_name}}")

  new_hash <- tryCatch(
    {
      if (is.null(source_path) || !file.exists(source_path)) {
        cli::cli_alert_danger(
          "Archive source not found: {.path {source_path}}"
        )
        NA_character_
      } else {
        att_hash(source_path)
      }
    },
    error = function(e) {
      cli::cli_alert_danger(
        "Failed to hash archive {.file {archive_name}}: {conditionMessage(e)}"
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


#' Re-read a local archive for refresh, extract and compare individual files
#'
#' Used by `att_refresh()` for local archive sources. Hashes the archive at
#' its `source_path`, and if changed, extracts and returns per-file comparison
#' records.
#' @noRd
refresh_fetch_local_archive <- function(archive_name, archive_info,
                                        file_records, tmp_dir) {
  source_path <- archive_info$source_path
  recorded_hash <- archive_info$sha256 %||% NA_character_

  cli::cli_alert("Checking archive {.file {archive_name}}")

  # Read failed — mark all files as error
  if (is.null(source_path) || !file.exists(source_path)) {
    cli::cli_alert_danger(
      "Archive source not found: {.path {source_path}}"
    )
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
      files = error_entries
    ))
  }

  new_hash <- att_hash(source_path)
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
      files = unchanged_entries
    ))
  }

  # Archive changed — extract and compare individual files
  extract_dir <- file.path(tmp_dir, "extracted")
  dir.create(extract_dir, showWarnings = FALSE)

  type <- archive_type(source_path)
  extract_archive(source_path, extract_dir, type)

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
    files = file_comparisons
  )
}
