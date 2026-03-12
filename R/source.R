#' Define a Data Source
#'
#' Creates an `att_source` object describing a data source: its landing page,
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
#' @param data_paths Character vector of local file paths for data files. Use a
#'   named vector to control filenames in the store (e.g.,
#'   `c(survey.csv = "~/data/raw-survey.csv")`). Unnamed entries use the
#'   source basename. Used with [att_register()].
#' @param metadata_paths Character vector of local file paths for metadata
#'   files (data dictionaries, codebooks, etc.). Named vector for custom
#'   filenames, same as `data_paths`. Used with [att_register()].
#' @param title Title of the data source (for citation).
#' @param publisher Publisher or institution name (for citation).
#' @param year Publication or release year (for citation).
#' @param author Author name (for citation). If not set, `publisher` is used.
#' @param format Data format description (e.g., `"CSV"`, `"Shapefile"`,
#'   `"File Geodatabase"`). Used in APA-style citations as
#'   `[Data set; Format]`.
#' @param description Brief description of the data source.
#' @param metadata Named list of additional metadata fields. Merged with
#'   `title`, `publisher`, `year`, and `description`.
#' @return An object of class `att_source`.
#' @export
#' @examples
#' src <- att_source(
#'   name = "example-data",
#'   landing_url = "https://example.com/data",
#'   data_urls = c(main = "https://example.com/data.csv"),
#'   metadata_urls = c(codebook = "https://example.com/codebook.pdf"),
#'   title = "Example Dataset",
#'   publisher = "Example Agency",
#'   year = "2025"
#' )
#' src
att_source <- function(name,
                       landing_url = NULL,
                       data_urls = NULL,
                       metadata_urls = NULL,
                       data_paths = NULL,
                       metadata_paths = NULL,
                       title = NULL,
                       publisher = NULL,
                       year = NULL,
                       author = NULL,
                       format = NULL,
                       description = NULL,
                       metadata = list()) {
  if (!is.character(name) || length(name) != 1 || nchar(name) == 0) {
    cli::cli_abort("{.arg name} must be a non-empty single character string.")
  }

  dir_name <- gsub("[^a-zA-Z0-9_-]", "-", name)

  has_urls <- !is.null(data_urls) || !is.null(metadata_urls)
  has_paths <- !is.null(data_paths) || !is.null(metadata_paths)
  if (has_urls && has_paths) {
    cli::cli_abort(c(
      "A source cannot mix remote URLs and local paths.",
      "i" = "Use {.arg data_urls}/{.arg metadata_urls} for remote sources ({.fun att_download}).",
      "i" = "Use {.arg data_paths}/{.arg metadata_paths} for local sources ({.fun att_register})."
    ))
  }

  # Merge explicit metadata fields with the metadata list
  meta <- c(
    list(title = title, publisher = publisher, year = year,
         author = author, format = format, description = description),
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
      data_paths = data_paths,
      metadata_paths = metadata_paths,
      metadata = meta
    ),
    class = "att_source"
  )
}

#' @export
print.att_source <- function(x, ...) {
  cli::cli_h3("attest source: {.val {x$name}}")

  if (!is.null(x$landing_url)) {
    cli::cli_alert_info("Landing page: {.url {x$landing_url}}")
  }

  origin <- if (!is.null(x$data_paths) || !is.null(x$metadata_paths)) {
    "local"
  } else {
    "remote"
  }
  cli::cli_alert_info("Origin: {.strong {origin}}")

  n_data <- length(x$data_urls) + length(x$data_paths)
  n_meta <- length(x$metadata_urls) + length(x$metadata_paths)
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
