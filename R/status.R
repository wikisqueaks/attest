#' View Source Status Summary
#'
#' Prints a summary table of all tracked sources in the store by scanning
#' for `_attest/provenance.json` files.
#'
#' @param store Path to the store. Defaults to [att_store()].
#' @return A data frame of source information.
#' @export
#' @examples
#' \dontrun{
#' att_status()
#' }
att_status <- function(store = NULL) {
  if (is.null(store)) store <- att_store()

  if (!dir.exists(store)) {
    cli::cli_alert_info("Store directory not found at {.path {store}}")
    return(data.frame())
  }

  # Scan for _attest/provenance.json files
  prov_files <- list.files(
    store,
    pattern = "^provenance\\.json$",
    recursive = TRUE,
    full.names = TRUE
  )
  # Only keep those inside _attest/ subdirectories
  prov_files <- prov_files[grepl("_attest", prov_files, fixed = TRUE)]

  if (length(prov_files) == 0) {
    cli::cli_alert_info("No sources found in {.path {store}}")
    return(data.frame())
  }

  rows <- lapply(prov_files, function(pf) {
    prov <- jsonlite::read_json(pf)

    n_files <- length(prov$files)
    total_size <- sum(
      vapply(prov$files, function(f) {
        as.numeric(f$size %||% 0)
      }, numeric(1)),
      na.rm = TRUE
    )

    data.frame(
      name = prov$name %||% NA_character_,
      origin = prov$origin %||% "remote",
      created = prov$created %||% NA_character_,
      last_updated = prov$last_updated %||% NA_character_,
      n_files = n_files,
      total_bytes = total_size,
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, rows)
}

#' Read a Source's Provenance Record
#'
#' Reads the `provenance.json` file for a given source and returns it as a
#' list.
#'
#' @param source A source name (character) or [att_source()] object.
#' @param store Path to the store. Defaults to [att_store()].
#' @return A list containing the full provenance record.
#' @export
#' @examples
#' \dontrun{
#' att_read_provenance("my-source")
#' }
att_read_provenance <- function(source, store = NULL) {
  name <- resolve_source_name(source)
  if (is.null(store)) store <- att_store()

  prov_path <- provenance_path(store, name)
  if (!file.exists(prov_path)) {
    cli::cli_abort("No provenance record found for {.val {name}}.")
  }

  jsonlite::read_json(prov_path)
}
