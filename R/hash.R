#' Compute a File Hash
#'
#' Computes a cryptographic hash of a file using SHA-256 (default) or another
#' algorithm supported by [digest::digest()].
#'
#' @param path Path to the file.
#' @param algo Hash algorithm. Default is `"sha256"`.
#' @return The hash as a character string.
#' @export
#' @examples
#' tf <- tempfile()
#' writeLines("hello world", tf)
#' att_hash(tf)
att_hash <- function(path, algo = "sha256") {

  if (!file.exists(path)) {
    cli::cli_abort("File not found: {.path {path}}")
  }

  digest::digest(path, algo = algo, file = TRUE)
}
