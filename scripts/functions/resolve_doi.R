#' Resolve a DOI using doi.org
#'
#' Resolve a DOI to a URL via [doi.org](https://doi.org/). This is slower than using the [REST API](https://www.doi.org/factsheets/DOIProxy.html#rest-api), however returns the final, redirected URL.
#'
#' @param doi Character. A publication doi with or without a preceding URL.
#'
#'#' @return S3 Class object \code{doi} with \code{doi} and \code{url}, as well as \code{success} (binary) and \code{error} (details on error if not \code{success}), and raw \code{response}.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' resolve_doi("10.1002/bjs.9387")
#' }
#'
resolve_doi <- function(doi) {

  # Remove scheme and hostname, if supplied
  doi <- httr::parse_url(doi)$path

  resp <- httr::HEAD("https://doi.org", path = doi)

  # Check for errors (status code)
  if (httr::http_error(resp)){
    error <-  resp$status_code
    success <-  FALSE
    url <-  NA
  } else {
    # If no errors, get url
    url <- resp$url
    success <- TRUE
    error <- NA
  }

  structure(
    list(
      doi = doi,
      url = url,
      success = success,
      error = error,
      response = resp
    ),
    class = "doi"
  )
}

print.doi <- function(x, ...) {
  cat("<DOI ", x$doi, ">\n", sep = "")
  if (x$success) str(x$url)
  if (!x$success) cat("Error:", x$error)
  invisible(x)
}
