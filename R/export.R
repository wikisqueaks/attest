#' Export a Portable Manifest of All Sources
#'
#' Reads every provenance record in the store and writes a single JSON
#' manifest capturing the information needed to recreate each source. The
#' manifest is a portable recipe — it records URLs, local paths, metadata,
#' and archive classifications, but no file contents.
#'
#' The manifest is intended to be version-controlled and shared across
#' projects. Use [att_import()] in a new project to generate and execute
#' a data-acquisition script from the manifest.
#'
#' @param path Output file path. Defaults to `attest-manifest.json` in the
#'   current working directory.
#' @param store Path to the provenance store. Defaults to [att_store()].
#' @return The manifest (a list), invisibly.
#' @seealso [att_import()] to replay the manifest in another project.
#' @export
#' @examples
#' \dontrun{
#' att_export()
#' att_export("my-sources.json")
#' }
att_export <- function(path = "attest-manifest.json", store = NULL) {
  if (is.null(store)) store <- att_store()

  if (!dir.exists(store)) {
    cli::cli_abort("Store directory not found at {.path {store}}.")
  }

  prov_files <- list.files(
    store,
    pattern = "^provenance\\.json$",
    recursive = TRUE,
    full.names = TRUE
  )
  prov_files <- prov_files[grepl("_attest", prov_files, fixed = TRUE)]

  # Exclude archived provenance (inside archive/ directories)
  prov_files <- prov_files[!grepl("archive/", prov_files, fixed = TRUE)]

  if (length(prov_files) == 0) {
    cli::cli_abort("No sources found in {.path {store}}.")
  }

  sources <- list()

  for (pf in prov_files) {
    prov <- jsonlite::read_json(pf)
    entry <- export_source_entry(prov)
    sources <- c(sources, list(entry))
  }

  manifest <- list(
    manifest_type = "attest_manifest",
    generated = timestamp_now(),
    attest_version = as.character(utils::packageVersion("attest")),
    sources = sources
  )

  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  jsonlite::write_json(
    manifest, path,
    pretty = TRUE, auto_unbox = TRUE
  )

  cli::cli_alert_success(
    "Exported {length(sources)} source{?s} to {.path {path}}"
  )

  invisible(manifest)
}


#' Build a manifest entry from a provenance record
#' @noRd
export_source_entry <- function(prov) {
  origin <- prov$origin %||% "remote"
  name <- prov$name

  entry <- list(
    name = name,
    origin = origin,
    landing_url = prov$landing_url,
    metadata = prov$metadata
  )

  if (origin == "remote") {
    entry <- c(entry, export_remote_source(prov))
  } else {
    entry <- c(entry, export_local_source(prov))
  }

  entry
}


#' Extract URL mappings and classify info from a remote provenance record
#' @noRd
export_remote_source <- function(prov) {
  files <- prov$files %||% list()
  archives <- prov$archives %||% list()

  # Determine which files came from archives
  archive_files <- character(0)
  for (fname in names(files)) {
    if (!is.null(files[[fname]]$extracted_from)) {
      archive_files <- c(archive_files, fname)
    }
  }

  data_urls <- list()
  metadata_urls <- list()

  for (fname in names(files)) {
    fi <- files[[fname]]
    if (fname %in% archive_files) next
    location <- fi$location %||% "root"
    url <- fi$url
    if (is.null(url)) next

    if (location == "metadata") {
      metadata_urls[[fname]] <- url
    } else {
      data_urls[[fname]] <- url
    }
  }

  # Add archive URLs to data_urls
  for (aname in names(archives)) {
    url <- archives[[aname]]$url
    if (!is.null(url)) {
      data_urls[[aname]] <- url
    }
  }

  result <- list(
    data_urls = data_urls,
    metadata_urls = metadata_urls
  )

  if (length(result$data_urls) == 0) result$data_urls <- NULL
  if (length(result$metadata_urls) == 0) result$metadata_urls <- NULL

  # Reconstruct classify from archive file placements
  if (length(archive_files) > 0) {
    classify <- reconstruct_classify(files, archive_files)
    if (!is.null(classify)) {
      result$classify <- classify
    }
  }

  result
}


#' Extract path mappings from a local provenance record
#' @noRd
export_local_source <- function(prov) {
  files <- prov$files %||% list()

  data_paths <- list()
  metadata_paths <- list()

  for (fname in names(files)) {
    fi <- files[[fname]]
    location <- fi$location %||% "root"
    source_path <- fi$source_path
    if (is.null(source_path)) next

    if (location == "metadata") {
      metadata_paths[[fname]] <- source_path
    } else {
      data_paths[[fname]] <- source_path
    }
  }

  result <- list(
    data_paths = data_paths,
    metadata_paths = metadata_paths
  )

  if (length(result$data_paths) == 0) result$data_paths <- NULL
  if (length(result$metadata_paths) == 0) result$metadata_paths <- NULL

  result
}


#' Reconstruct a classify list from extracted file records
#'
#' Groups archive-extracted files by their extension and location to produce
#' a `classify` argument suitable for [att_download()].
#' @noRd
reconstruct_classify <- function(files, archive_files) {
  data_exts <- character(0)
  metadata_exts <- character(0)

  for (fname in archive_files) {
    fi <- files[[fname]]
    ext <- paste0(".", tolower(tools::file_ext(fname)))
    if (ext == ".") next

    location <- fi$location %||% "root"
    if (location == "metadata") {
      metadata_exts <- c(metadata_exts, ext)
    } else {
      data_exts <- c(data_exts, ext)
    }
  }

  data_exts <- unique(data_exts)
  metadata_exts <- unique(metadata_exts)

  # Remove from data_exts anything that also appears in metadata
  # (metadata classification wins — it's the non-default)
  data_exts <- setdiff(data_exts, metadata_exts)

  classify <- list()
  if (length(data_exts) > 0) classify$data <- data_exts
  if (length(metadata_exts) > 0) classify$metadata <- metadata_exts

  if (length(classify) == 0) return(NULL)
  classify
}
