#' Remove a Source from the Store
#'
#' Deletes a source's directory (data files, metadata, provenance, and
#' archives) and removes its BibTeX entry from `data-sources.bib`.
#'
#' In interactive sessions, prompts for confirmation before deleting. Use
#' `force = TRUE` to skip the prompt (e.g., in scripts).
#'
#' @param source A source name (character) or [att_source()] object.
#' @param store Path to the provenance store. Defaults to [att_store()].
#' @param force Logical; if `TRUE`, skip the confirmation prompt.
#' @return `TRUE` if the source was removed, `FALSE` if cancelled. Returned
#'   invisibly.
#' @export
#' @examples
#' \dontrun{
#' att_remove("my-source")
#' att_remove("my-source", force = TRUE)
#' }
att_remove <- function(source, store = NULL, force = FALSE) {
  name <- resolve_source_name(source)
  if (is.null(store)) store <- att_store()

  source_dir <- file.path(store, name)

  if (!dir.exists(source_dir)) {
    cli::cli_abort("Source {.val {name}} not found in {.path {store}}.")
  }

  # Summarise what will be deleted
  all_files <- list.files(source_dir, recursive = TRUE)
  n_files <- length(all_files)
  total_size <- sum(file.size(file.path(source_dir, all_files)), na.rm = TRUE)
  size_label <- format_size(total_size)

  if (!force) {
    if (!rlang::is_interactive()) {
      cli::cli_abort(c(
        "Cannot remove source {.val {name}} without confirmation.",
        "i" = "Use {.code force = TRUE} to skip the prompt."
      ))
    }

    cli::cli_alert_warning(
      "This will permanently delete {.val {name}}: {n_files} file{?s} ({size_label})"
    )
    answer <- readline("Are you sure? (y/N): ")
    if (!tolower(trimws(answer)) %in% c("y", "yes")) {
      cli::cli_alert_info("Cancelled.")
      return(invisible(FALSE))
    }
  }

  # Remove bib entry
  bib_path <- file.path(store, "data-sources.bib")
  if (file.exists(bib_path)) {
    bib_key <- gsub("[^a-zA-Z0-9]", "_", name)
    remove_bib_entry(bib_path, bib_key)
    sync_markdown_citations(bib_path)
  }

  # Delete source directory
  unlink(source_dir, recursive = TRUE)

  if (dir.exists(source_dir)) {
    cli::cli_abort("Failed to delete {.path {source_dir}}.")
  }

  cli::cli_alert_success("Removed source {.val {name}}")
  invisible(TRUE)
}


#' Format file size in human-readable units
#' @noRd
format_size <- function(bytes) {
  if (is.na(bytes) || bytes == 0) return("0 B")
  units <- c("B", "KB", "MB", "GB")
  i <- min(floor(log(bytes, 1024)), length(units) - 1)
  paste0(round(bytes / 1024^i, 1), " ", units[i + 1])
}


#' Remove a BibTeX entry by key from a .bib file
#' @noRd
remove_bib_entry <- function(bib_path, key) {
  if (!file.exists(bib_path)) return(invisible(NULL))

  existing <- readLines(bib_path, warn = FALSE)
  content <- paste(existing, collapse = "\n")

  escaped_key <- gsub("([.])", "\\\\\\1", key)
  pattern <- paste0("@\\w+\\{", escaped_key, ",(?s).*?\\n\\}")
  content <- gsub(pattern, "", content, perl = TRUE)

  # Clean up multiple blank lines

  content <- gsub("\n{3,}", "\n\n", trimws(content))

  if (nchar(content) == 0) {
    file.remove(bib_path)
  } else {
    writeLines(content, bib_path)
  }

  invisible(NULL)
}
