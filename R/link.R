#' Link a Remote Source Without Downloading
#'
#' Records provenance for a remote data source that is accessed live — a REST
#' API, a data portal query, or any URL you call rather than download a file
#' from. Creates the standard store directory structure and writes a
#' `provenance.json` record with `origin = "link"`, but downloads nothing.
#'
#' A `.url` Internet Shortcut file is written to the source directory so the
#' endpoint is easy to open directly from a file browser.
#'
#' An HTTP HEAD request is attempted against the primary URL (first of
#' `data_urls`, or `landing_url` if no `data_urls` are set) to record the
#' HTTP status and content type at link time. This is best-effort: if the
#' request fails, a warning is issued but the provenance record is still
#' written.
#'
#' `att_link()` is intended for first-time registration. If a provenance
#' record already exists for this source, it will error and recommend
#' [att_update()] instead.
#'
#' @param source An [att_source()] object. Must have at least one of
#'   `data_urls`, `metadata_urls`, or `landing_url` set. Must not have local
#'   paths (`data_paths`/`metadata_paths`).
#' @param store Path to the provenance store. Defaults to [att_store()].
#' @param cite Logical; if `TRUE` (default), generate a BibTeX citation and
#'   append it to `data-sources.bib` in the store root.
#' @return A list containing the full provenance record, invisibly.
#' @export
#' @examples
#' \dontrun{
#' src <- att_source(
#'   name = "earthquake-feed",
#'   data_urls = "https://earthquake.usgs.gov/fdsnws/event/1/query?format=geojson",
#'   landing_url = "https://earthquake.usgs.gov/fdsnws/event/1/",
#'   title = "USGS Earthquake Feed",
#'   publisher = "U.S. Geological Survey",
#'   year = "2026"
#' )
#' att_link(src)
#' }
att_link <- function(source, store = NULL, cite = TRUE) {
  if (!inherits(source, "att_source")) {
    cli::cli_abort("{.arg source} must be an {.cls att_source} object.")
  }

  if (!is.null(source$data_paths) || !is.null(source$metadata_paths)) {
    cli::cli_abort(c(
      "{.fun att_link} is for remote sources with URLs.",
      "i" = "This source has local paths. Use {.fun att_register} instead."
    ))
  }

  primary_url <- source$data_urls[[1]] %||% source$metadata_urls[[1]] %||%
    source$landing_url
  if (is.null(primary_url)) {
    cli::cli_abort(c(
      "Source {.val {source$name}} has no URLs to link to.",
      "i" = "Set {.arg data_urls}, {.arg metadata_urls}, or {.arg landing_url} in {.fun att_source}."
    ))
  }

  if (is.null(store)) store <- att_store()

  source_dir <- file.path(store, source$dir_name)
  provenance_dir <- file.path(source_dir, "_attest")
  existing_prov <- file.path(provenance_dir, "provenance.json")

  if (file.exists(existing_prov)) {
    cli::cli_abort(c(
      "Source {.val {source$name}} has already been linked.",
      "i" = "Use {.fun att_update} to edit its metadata."
    ))
  }

  metadata_dir <- file.path(source_dir, "metadata")

  dir.create(source_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(metadata_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(provenance_dir, recursive = TRUE, showWarnings = FALSE)

  created <- timestamp_now()

  # Attempt HEAD request on the primary URL (best-effort)
  endpoint <- list(linked = created)
  cli::cli_alert_info("Checking {.url {primary_url}}")
  resp <- tryCatch(
    att_request(primary_url) |>
      httr2::req_method("HEAD") |>
      httr2::req_perform(),
    error = function(e) {
      cli::cli_alert_warning(
        "Could not reach {.url {primary_url}}: {conditionMessage(e)}"
      )
      NULL
    }
  )
  if (!is.null(resp)) {
    endpoint$http_status <- httr2::resp_status(resp)
    ct <- httr2::resp_header(resp, "Content-Type")
    if (!is.null(ct)) endpoint$http_content_type <- ct
  }

  provenance <- list(
    name = source$name,
    dir_name = source$dir_name,
    origin = "link",
    data_urls = as.list(source$data_urls),
    metadata_urls = as.list(source$metadata_urls),
    landing_url = source$landing_url,
    metadata = source$metadata,
    endpoint = endpoint,
    files = list(),
    created = created,
    last_updated = timestamp_now(),
    attest_version = as.character(utils::packageVersion("attest"))
  )

  jsonlite::write_json(
    provenance,
    file.path(provenance_dir, "provenance.json"),
    pretty = TRUE, auto_unbox = TRUE
  )

  # Write .url Internet Shortcut for easy browser access
  url_file <- file.path(source_dir, paste0(source$dir_name, ".url"))
  writeLines(c("[InternetShortcut]", paste0("URL=", primary_url)), url_file)

  if (cite) {
    att_cite(source, store = store)
  }

  cli::cli_alert_success(
    "Source {.val {source$name}} linked at {.path {source_dir}}"
  )

  invisible(provenance)
}
