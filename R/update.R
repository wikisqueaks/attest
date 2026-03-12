#' Update Source Metadata
#'
#' Modifies the provenance record for a source and regenerates its BibTeX
#' citation. Use this to correct mistakes (e.g., a wrong landing URL or
#' title) without manually editing `provenance.json`.
#'
#' Only metadata and landing URL fields can be updated. File-level fields
#' (hashes, sizes, download timestamps) are managed by [att_download()],
#' [att_refresh()], and related functions.
#'
#' @param source A source name (character) or [att_source()] object.
#' @param store Path to the provenance store. Defaults to [att_store()].
#' @param landing_url New landing URL for the source.
#' @param title New title.
#' @param publisher New publisher name.
#' @param author New author name. If not set, `publisher` is used for
#'   citations.
#' @param year New publication year.
#' @return The updated provenance record, invisibly.
#' @export
#' @examples
#' \dontrun{
#' att_update("my-source", title = "Corrected Title")
#' att_update("my-source",
#'   landing_url = "https://correct-url.example.com",
#'   publisher = "Correct Agency"
#' )
#' }
att_update <- function(source, store = NULL, landing_url = NULL, title = NULL,
                       publisher = NULL, author = NULL, year = NULL) {
  name <- resolve_source_name(source)
  if (is.null(store)) store <- att_store()

  prov_path <- provenance_path(store, name)
  if (!file.exists(prov_path)) {
    cli::cli_abort("No provenance record found for {.val {name}}.")
  }

  updates <- list(
    landing_url = landing_url,
    title = title,
    publisher = publisher,
    author = author,
    year = year
  )
  updates <- Filter(Negate(is.null), updates)

  if (length(updates) == 0) {
    cli::cli_alert_info("Nothing to update.")
    prov <- jsonlite::read_json(prov_path)
    return(invisible(prov))
  }

  prov <- jsonlite::read_json(prov_path)

  # Apply updates
  if (!is.null(updates$landing_url)) {
    old <- prov$landing_url %||% "(none)"
    prov$landing_url <- updates$landing_url
    cli::cli_alert_info("landing_url: {.url {old}} \u2192 {.url {updates$landing_url}}")
  }

  metadata_fields <- c("title", "publisher", "author", "year")
  if (is.null(prov$metadata)) prov$metadata <- list()

  for (field in metadata_fields) {
    if (!is.null(updates[[field]])) {
      old <- prov$metadata[[field]] %||% "(none)"
      prov$metadata[[field]] <- updates[[field]]
      cli::cli_alert_info("{field}: {.val {old}} \u2192 {.val {updates[[field]]}}")
    }
  }

  prov$last_updated <- timestamp_now()

  jsonlite::write_json(
    prov, prov_path,
    pretty = TRUE, auto_unbox = TRUE
  )

  cli::cli_alert_success("Updated provenance for {.val {name}}")

  # Regenerate bib entry
  att_cite(name, store = store, write = TRUE)

  invisible(prov)
}
