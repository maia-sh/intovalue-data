#' Correct PDF urls based on publisher
#'
#' @param url Character. A publication URL. Usually generated from \code{resolve_doi}.
#'
#' @return A URL
#' @export
#'
correct_pdf_url <- function(url) {

  parsed <- httr::parse_url(url)

  # Determine publisher
  publisher <-  stringr::str_extract(parsed$hostname, c(
    "onlinelibrary.wiley.com",
    "ascopubs.org",
    "linkinghub.elsevier.com"
  )) %>%
    purrr::discard(is.na)

  # If there was more than 1 publisher match, treat as "other" and flag in info
  # Unspecified publishers are categorized as "other"
  if (length(publisher) > 1) {
    info <- "There seems to be multiple publishers"
    publisher <- "other"
  } else if (rlang::is_empty(publisher)) {
    publisher <- "other"
  }

  parsed <-
    switch(publisher,
           "onlinelibrary.wiley.com" = build_wiley(parsed),
           "ascopubs.org" = build_asco(parsed),
           "linkinghub.elsevier.com" = build_elsevier(parsed),
           "other" = parsed
    )

  if (publisher != "other"){
    url <- httr::build_url(parsed)
  }

  url

}

build_elsevier <- function(parsed){
  parsed$hostname <- "www.sciencedirect.com"
  parsed$path <-
    stringr::str_c(
      "science/article",
      stringr::str_extract(parsed$path, "/pii/.+$"),
      "/pdfft"
    )
  parsed$query <- list(isDTMRedir = "true", download = "true")

  parsed
}

build_wiley <- function(parsed){
  parsed$path <- stringr::str_replace(parsed$path, "full", "pdfdirect")
  parsed
}

build_asco <- function(parsed){
  parsed$path <- stringr::str_replace(parsed$path, "doi", "doi/pdfdirect")
  parsed
}
