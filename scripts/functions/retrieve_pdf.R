retrieve_pdf <- function(doi,
                         email,
                         dir = here::here(),
                         overwrite = FALSE,
                         source = "unpaywall",
                         sleep = 1/3){

  # Check for valid DOI
  if (!stringr::str_detect(doi, "^10\\.\\d{4,9}/[-.;()/:\\w]+$")){
    rlang::abort(glue::glue("`doi` must be a properly formated doi, not {doi}"))
  }

  # Check for valid source(s)
  sources <- c("unpaywall", "publisher", "magic")

  sources_txt <-
    glue::glue_collapse(glue::glue("'{sources}'"), sep = ", ", last = ", or ")

  if (!all(source %in% sources)){
    rlang::abort(glue::glue("Each `source` must be in {sources_txt}"))
  }

  # Create directory, if it doesn't exist
  fs::dir_create(dir)

  # Construct filepath
  filepath <- fs::path(dir, stringr::str_replace_all(doi, "/", "\\+"), ext = "pdf")

  # If file exists and not overwriting, don't download, and return NULL
  if (!overwrite && fs::file_exists(filepath)){
    rlang::inform(glue::glue("{doi}: Already downloaded"))
    return(NULL)
  }

  Sys.sleep(sleep)

  # Try unpaywall
  if ("unpaywall" %in% source){

    oa_url <- get_pdf_url_unpaywall(doi, email)

    # If link, download and check valid pdf, and if good, then return
    if(!is.na(oa_url)){
      download.file(oa_url, filepath, mode = "wb", quiet = TRUE)

      if(check_exists_length(filepath) > 0){
        rlang::inform(glue::glue("{doi}: Successfully downloaded via unpaywall"))
        return()
      } else (fs::file_delete(filepath))
    }
  }

  # Try publisher (via doi.org)
  if ("publisher" %in% source){
    doi_url <- resolve_doi(doi)$url

    if(!is.na(doi_url)){

      # Edit pdf link per manual rules
      doi_url <- correct_pdf_url(doi_url)
      download.file(doi_url, filepath, mode = "wb", quiet = TRUE)

      if(check_exists_length(filepath) > 0){
        rlang::inform(glue::glue("{doi}: Successfully downloaded via publisher"))
        return()
      } else (fs::file_delete(filepath))
    }
  }

  # Try magic
  if ("magic" %in% source){
    magic_url <- get_pdf_url_magic(doi)
    if(!is.na(magic_url)){

      download.file(magic_url, filepath, mode = "wb", quiet = TRUE)

      if(check_exists_length(filepath) > 0){
        rlang::inform(glue::glue("{doi}: Successfully downloaded via magic"))
        return()
      } else (fs::file_delete(filepath))
    }
  }

  rlang::inform(glue::glue("{doi}: Not downloaded"))
}

check_exists_length <- function(x) {
  tryCatch(
    error = function(cnd) 0,
    qpdf::pdf_length(x)
  )
}

get_pdf_url_unpaywall <- function(doi, email){

  safely_oadoi_fetch <- purrr::safely(roadoi::oadoi_fetch)

  response <- safely_oadoi_fetch(doi, email)

  oa_tbl <-
    # pluck(response, "best_oa_location")
    response$result$best_oa_location[[1]]

  if (!rlang::is_null(oa_tbl) && nrow(oa_tbl) == 1 && "url_for_pdf" %in% colnames(oa_tbl)){
    out <- pull(oa_tbl, url_for_pdf)
  } else {out <- NA_character_}
}

get_pdf_url_magic <- function (doi) {
  out <- tryCatch({

    url <- paste0(
      "https://sci-hub.se/",
      doi
    )

    index <- xml2::read_html(url)

    linkjs <- index %>%
      rvest::html_node("div#buttons a") %>%
      rvest::html_attr("onclick")

    linkjs %>%
      substr(16, nchar(linkjs)) %>%
      substr(1, nchar(linkjs) - 16 - 14) %>%
      return()

  },
  error=function(cond) {
    message(paste("DOI did not resolve:", doi))
    message("Here's the original error message:")
    message(cond)

    return (NA)
  },
  warning=function(cond) {
    message(paste("DOI caused a warning:", doi))
    message("Here's the original warning message:")
    message(cond)

    return (NA)
  },
  finally={
  })

  return(out)

}
