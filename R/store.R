#' Get or Set the Active Store Path
#'
#' Retrieves or sets the path to the directory where attested data sources are
#' stored. Resolution order: explicit `path` argument, then `attest.store`
#' option, then `ATTEST_STORE` environment variable, then `"data/raw"` in the
#' current working directory.
#'
#' @param path If provided, sets this as the active store path (via
#'   `options(attest.store = path)`). If `NULL`, returns the current store
#'   path.
#' @return The store path (character), invisibly when setting.
#' @export
#' @examples
#' # Get the current store path
#' att_store()
#'
#' # Set a custom store path
#' att_store("/path/to/my/store")
att_store <- function(path = NULL) {
  if (!is.null(path)) {
    path <- normalizePath(path, mustWork = FALSE)
    options(attest.store = path)
    return(invisible(path))
  }

  store <- getOption("attest.store")
  if (!is.null(store)) return(store)

  store <- Sys.getenv("ATTEST_STORE", unset = "")
  if (nchar(store) > 0) return(store)

  file.path(getwd(), "data", "raw")
}
