#' Get or Set the Active Store Path
#'
#' Retrieves or sets the path to the directory where acquired data sources are
#' stored. Resolution order: explicit `path` argument, then `acquire.store`
#' option, then `ACQUIRE_STORE` environment variable, then `"data/raw"` in the
#' current working directory.
#'
#' @param path If provided, sets this as the active store path (via
#'   `options(acquire.store = path)`). If `NULL`, returns the current store
#'   path.
#' @return The store path (character), invisibly when setting.
#' @export
#' @examples
#' # Get the current store path
#' acq_store()
#'
#' # Set a custom store path
#' acq_store("/path/to/my/store")
acq_store <- function(path = NULL) {
  if (!is.null(path)) {
    path <- normalizePath(path, mustWork = FALSE)
    options(acquire.store = path)
    return(invisible(path))
  }

  store <- getOption("acquire.store")
  if (!is.null(store)) return(store)

  store <- Sys.getenv("ACQUIRE_STORE", unset = "")
  if (nchar(store) > 0) return(store)

  file.path(getwd(), "data", "raw")
}
