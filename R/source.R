#' Define a Data Source
#'
#' Creates an `acq_source` object describing a data source: its landing page,
#' data file URLs, metadata file URLs, and descriptive metadata.
#'
#' @param name A short, human-readable name for this source. Used as the
#'   directory name (non-alphanumeric characters replaced with `-`).
#' @param landing_url URL of the landing or metadata page for this source.
#' @param data_urls Character vector of URLs for data files. Use a named vector
#'   to control local filenames (e.g., `c(mydata.csv = "https://...")`).
#'   Unnamed entries use the URL basename as the filename. Data files are saved
#'   directly in the source directory.
#' @param metadata_urls Character vector of URLs for metadata files (data
#'   dictionaries, codebooks, PDFs, etc.). Named vector for custom filenames,
#'   same as `data_urls`. Metadata files are saved in a `metadata/`
#'   subdirectory.
#' @param title Title of the data source (for citation).
#' @param publisher Publisher or institution name (for citation).
#' @param year Publication or release year (for citation).
#' @param description Brief description of the data source.
#' @param metadata Named list of additional metadata fields. Merged with
#'   `title`, `publisher`, `year`, and `description`.
#' @return An object of class `acq_source`.
#' @export
#' @examples
#' src <- acq_source(
#'   name = "example-data",
#'   landing_url = "https://example.com/data",
#'   data_urls = c(main = "https://example.com/data.csv"),
#'   metadata_urls = c(codebook = "https://example.com/codebook.pdf"),
#'   title = "Example Dataset",
#'   publisher = "Example Agency",
#'   year = "2025"
#' )
#' src
acq_source <- function(name,
                       landing_url = NULL,
                       data_urls = NULL,
                       metadata_urls = NULL,
                       title = NULL,
                       publisher = NULL,
                       year = NULL,
                       description = NULL,
                       metadata = list()) {
  if (!is.character(name) || length(name) != 1 || nchar(name) == 0) {
    cli::cli_abort("{.arg name} must be a non-empty single character string.")
  }

  dir_name <- gsub("[^a-zA-Z0-9_-]", "-", name)

  # Merge explicit metadata fields with the metadata list
  meta <- c(
    list(title = title, publisher = publisher, year = year,
         description = description),
    metadata
  )

  meta <- meta[!vapply(meta, is.null, logical(1))]

  structure(
    list(
      name = name,
      dir_name = dir_name,
      landing_url = landing_url,
      data_urls = data_urls,
      metadata_urls = metadata_urls,
      metadata = meta
    ),
    class = "acq_source"
  )
}

#' @export
print.acq_source <- function(x, ...) {
  cli::cli_h3("acquire source: {.val {x$name}}")

  if (!is.null(x$landing_url)) {
    cli::cli_alert_info("Landing page: {.url {x$landing_url}}")
  }

  n_data <- length(x$data_urls)
  n_meta <- length(x$metadata_urls)
  cli::cli_alert_info("{n_data} data file{?s}, {n_meta} metadata file{?s}")

  if (length(x$metadata) > 0) {
    for (nm in names(x$metadata)) {
      val <- x$metadata[[nm]]
      if (nchar(val) > 60) val <- paste0(substr(val, 1, 57), "...")
      cli::cli_bullets(c(" " = "{.field {nm}}: {val}"))
    }
  }

  invisible(x)
}
