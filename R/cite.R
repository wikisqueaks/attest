#' Generate a BibTeX Citation
#'
#' Creates a BibTeX entry for a data source using its metadata and provenance
#' record. The access date, URL, and file hashes are populated automatically.
#'
#' When `write = TRUE`, the entry is appended to `data-sources.bib` in the
#' store root directory. If an entry with the same key already exists, it is
#' replaced.
#'
#' @param source An [att_source()] object or a source name (character). When a
#'   name is given, metadata is read from the provenance record.
#' @param store Path to the provenance store. Defaults to [att_store()].
#' @param key BibTeX citation key. Defaults to a sanitized version of the
#'   source name.
#' @param write Logical; if `TRUE` (default), write the entry to
#'   `data-sources.bib` in the store root.
#' @return The BibTeX entry as a character string, invisibly.
#' @export
#' @examples
#' \dontrun{
#' att_cite("my-source")
#' }
att_cite <- function(source, store = NULL, key = NULL, write = TRUE) {
  if (is.null(store)) store <- att_store()

  if (inherits(source, "att_source")) {
    name <- source$dir_name
    meta <- source$metadata
    landing_url <- source$landing_url
  } else {
    name <- resolve_source_name(source)
    meta <- list()
    landing_url <- NULL
  }

  # Read provenance record if available
  prov_path <- provenance_path(store, name)
  prov <- if (file.exists(prov_path)) jsonlite::read_json(prov_path) else list()

  # Fill metadata from provenance if not already set
  if (length(meta) == 0 && length(prov$metadata) > 0) {
    meta <- prov$metadata
  }

  if (is.null(landing_url)) {
    landing_url <- prov$landing_url
  }

  # Guard against empty lists from JSON null round-tripping
  if (length(landing_url) == 0) landing_url <- NULL

  # Build fields
  if (is.null(key)) {
    key <- gsub("[^a-zA-Z0-9]", "_", name)
  }

  title <- meta$title %||% name
  author <- meta$publisher %||% meta$author %||% "Unknown"
  year <- meta$year %||% format(Sys.Date(), "%Y")
  url <- landing_url

  # Access date from provenance or today

  urldate <- if (!is.null(prov$created)) {
    substr(prov$created, 1, 10)
  } else {
    format(Sys.Date(), "%Y-%m-%d")
  }

  # APA-style type/format note: [Data set] or [Data set; Format]
  data_format <- meta$format
  note <- if (!is.null(data_format)) {
    paste0("[Data set; ", data_format, "]")
  } else {
    "[Data set]"
  }

  fields <- list(
    title = title,
    author = paste0("{", author, "}"),
    year = year,
    note = note,
    url = url,
    urldate = urldate
  )

  bib_text <- build_bib_entry(key, type = "misc", fields = fields)

  if (write) {
    bib_path <- file.path(store, "data-sources.bib")
    update_bib_file(bib_path, key, bib_text)
    sync_markdown_citations(bib_path)
    cli::cli_alert_success("BibTeX entry {.val {key}} written to {.path {bib_path}}")
  }

  cli::cli_text("")
  cli::cli_verbatim(bib_text)

  invisible(bib_text)
}
