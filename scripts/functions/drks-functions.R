#' Get DRKS record
#'
#' Fetch DRKS record based on \code{drks_id} and check that it resolved to a valid, matching record.
#'
#' @param drks_id
#' @param quiet
#'
#' @return DRKS record as html_document, if valid, otherwise, NULL
#'
#' @examples
#' fetch_drks(drks_id)

fetch_drks <- function(drks_id, quiet = FALSE){

  drks_url <- "https://www.drks.de/drks_web/navigate.do?navigationId=trial.HTML&TRIAL_ID="

  doc <-
    rvest::read_html(glue::glue("{drks_url}{drks_id}"))

  doc_id <- get_drks_element(doc, ".drks_id p")

  # If drks_id does not resolve to valid record with same id, inform and return early
  if (rlang::is_empty(doc_id) || doc_id != drks_id) {
    if (!quiet){rlang::inform(glue::glue("{drks_id}: invalid drks id"))}
    return(NULL)
  }

  if (!quiet){rlang::inform(glue::glue("{drks_id}: fetched"))}
  return(doc)

}

#' Download DRKS
#'
#' Save a local copy of DRKS record. Calls \code{fetch_drks()} to fetch DRKS record, if needed.
#'
#' @param drks_id
#' @param quiet
#' @param overwrite
#' @param quiet
#'
#' @return DRKS record as html_document, if valid, otherwise, NULL
#'
#' @examples
#' \dontrun{download_drks(drks_id, dir, overwrite = TRUE)}

download_drks <- function(drks_id,
                          dir = here::here(),
                          overwrite = FALSE,
                          quiet = FALSE){

  drks_url <- "https://www.drks.de/drks_web/navigate.do?navigationId=trial.HTML&TRIAL_ID="

  # Create directory (if doesn't exist) and filepath
  dir_files <-
    fs::dir_create(dir) %>%
    fs::dir_ls()

  filepath <- glue::glue("{dir}/{drks_id}")

  # If record already downloaded and no rewrite, inform and return early
  if (filepath %in% dir_files & !overwrite){
    if (!quiet){rlang::inform(glue::glue("{drks_id}: already downloaded, not overwriting"))}
    doc <- rvest::read_html(filepath)
    return(doc)
  }

  # Get drks record
  doc <- fetch_drks(drks_id, quiet = quiet)

  # If no valid drks record, return early
  if (rlang::is_empty(doc)) {return(NULL)}

  # Save drks record
  xml2::download_html(glue::glue("{drks_url}{drks_id}"), filepath)

  if (!quiet){rlang::inform(glue::glue("{drks_id}: saved"))}

  return(doc)
}

#' Parse DRKS registration main details
#'
#' Currently a subset of details, could add additional.
#' Provide only one of \code{filepath} or \code{drks_id}
#'
#' @param filepath Character. Filepath including directory portion to DRKS hmtl_document (e.g., previously downloaded with \code{download_drks()}
#' @param drks_id Character. DRKS id to be fetched with \code{fetch_drks()}
#'
#' @return Tibble with columns for each detail

parse_drks_study <- function(filepath = NULL, drks_id = NULL){

  # Check parameters
  if (rlang::is_null(filepath) & rlang::is_null(drks_id)){
    rlang::abort("Neither `filename` nor `drks_id` provided. Must supply one.")
  }
  if (!rlang::is_null(filepath) & !rlang::is_null(drks_id)){
    rlang::abort("Both `filename` and `drks_id` provided. Supply only one.")
  }

  # Get drks html, depending on filepath or drks_id
  if (!rlang::is_null(drks_id)){
    doc <- fetch_drks(drks_id)

    # If no valid drks record, return early
    if (rlang::is_empty(doc)) {return(NULL)}

  } else {
    doc <- rvest::read_html(filepath)
  }

  out <- list()
  out$drks_id <- get_drks_element(doc, ".drks_id p")
  out$title <- stringr::str_c(get_drks_element(doc, "p.title"), collapse = " ")
  out$registration_date <- get_drks_date(doc, ".firstDrksPublishDate")
  out$start_date <- get_drks_date(doc, ".schedule")
  out$completion_date <- get_drks_date(doc, ".deadline")
  out$allocation <- get_drks_element(doc, ".allocation")
  out$study_type <- get_drks_element(doc, ".type")
  out$study_type_non_interventional <- get_drks_element(doc, ".typeNotInterventional")
  out$masking <- get_drks_element(doc, ".maskingType")
  out$masking_who <- get_drks_element(doc, ".maskingWho")
  out$phase <- get_drks_element(doc, ".phase")
  out$purpose <- get_drks_element(doc, ".purpose")
  out$running <- get_drks_element(doc, ".running")
  out$enrollment <- get_drks_element(doc, ".targetSize")
  out$centers <- get_drks_element(doc, ".monocenter")
  out$national <- get_drks_element(doc, ".national")
  out$recruitment_status <- get_drks_element(doc, "li.state")
  out$investigator_initiated <- get_drks_element(doc, ".investorInitiated")

  tibble::as_tibble(out)
}

#' Parse DRKS registration secondary ids
#'
#' @param filepath Character. Filepath including directory portion to DRKS hmtl_document (e.g., previously downloaded with \code{download_drks()}
#' @param drks_id Character. DRKS id to be fetched with \code{fetch_drks()}
#'
#' @return Tibble with `id_type` and `id` columns

parse_drks_ids <- function(filepath = NULL, drks_id = NULL){

  # Check parameters
  if (rlang::is_null(filepath) & rlang::is_null(drks_id)){
    rlang::abort("Neither `filename` nor `drks_id` provided. Must supply one.")
  }
  if (!rlang::is_null(filepath) & !rlang::is_null(drks_id)){
    rlang::abort("Both `filename` and `drks_id` provided. Supply only one.")
  }

  # Get drks html, depending on filepath or drks_id
  if (!rlang::is_null(drks_id)){
    doc <- fetch_drks(drks_id)

    # If no valid drks record, return early
    if (rlang::is_empty(doc)) {return(NULL)}

  } else {
    doc <- rvest::read_html(filepath)
  }

  out <- get_drks_labels_elements(doc, ".secondaryID")

  # If no ids, return NULL early
  if (rlang::is_null(out)) return(NULL)

  out %>%
    rename(id_type = label, id = text) %>%
    mutate(drks_id = get_drks_element(doc, ".drks_id p"), .before = 1)
}

#' Parse DRKS registration references
#'
#' @param filepath Character. Filepath including directory portion to DRKS hmtl_document (e.g., previously downloaded with \code{download_drks()}
#' @param drks_id Character. DRKS id to be fetched with \code{fetch_drks()}
#'
#' @return Tibble with `reference_type` and `citation` columns

parse_drks_references <- function(filepath = NULL, drks_id = NULL){

  # Check parameters
  if (rlang::is_null(filepath) & rlang::is_null(drks_id)){
    rlang::abort("Neither `filename` nor `drks_id` provided. Must supply one.")
  }
  if (!rlang::is_null(filepath) & !rlang::is_null(drks_id)){
    rlang::abort("Both `filename` and `drks_id` provided. Supply only one.")
  }

  # Get drks html, depending on filepath or drks_id
  if (!rlang::is_null(drks_id)){
    doc <- fetch_drks(drks_id)

    # If no valid drks record, return early
    if (rlang::is_empty(doc)) {return(NULL)}

  } else {
    doc <- rvest::read_html(filepath)
  }

  out <- get_drks_labels_elements_links(doc, ".publication")

  # If no ids, return NULL early
  if (rlang::is_null(out)) return(NULL)

  out %>%
    rename(reference_type = label, citation = text) %>%
    mutate(drks_id = get_drks_element(doc, ".drks_id p"), .before = 1)
}


#' Parse DRKS affiliations
#'
#' @param filepath Character. Filepath including directory portion to DRKS hmtl_document (e.g., previously downloaded with \code{download_drks()}
#' @param drks_id Character. DRKS id to be fetched with \code{fetch_drks()}
#'
#' @return Tibble with `affiliation_type` and `affiliation` columns


parse_drks_affiliations <- function(filepath = NULL, drks_id = NULL){

  # Check parameters
  if (rlang::is_null(filepath) & rlang::is_null(drks_id)){
    rlang::abort("Neither `filename` nor `drks_id` provided. Must supply one.")
  }
  if (!rlang::is_null(filepath) & !rlang::is_null(drks_id)){
    rlang::abort("Both `filename` and `drks_id` provided. Supply only one.")
  }

  # Get drks html, depending on filepath or drks_id
  if (!rlang::is_null(drks_id)){
    doc <- fetch_drks(drks_id)

    # If no valid drks record, return early
    if (rlang::is_empty(doc)) {return(NULL)}

  } else {
    doc <- rvest::read_html(filepath)
  }

  out <- get_drks_labels_elements_child(doc, "ul.addresses", "li.address-affiliation")

  # If no affiliations, return NULL early
  # if (rlang::is_null(out)) return(NULL)

  out %>%
    rename(affiliation_type = label, lead_affiliation = text) %>%
    mutate(drks_id = get_drks_element(doc, ".drks_id p"), .before = 1)

}


#' Parse DRKS registration facilities
#'
#' Provide only one of \code{filepath} or \code{drks_id}
#'
#' @param filepath Character. Filepath including directory portion to DRKS hmtl_document (e.g., previously downloaded with \code{download_drks()}
#' @param drks_id Character. DRKS id to be fetched with \code{fetch_drks()}
#'
#' @return Tibble with `facility_affiliation` column

parse_drks_facilities <- function(filepath = NULL, drks_id = NULL){

  # Check parameters
  if (rlang::is_null(filepath) & rlang::is_null(drks_id)){
    rlang::abort("Neither `filename` nor `drks_id` provided. Must supply one.")
  }
  if (!rlang::is_null(filepath) & !rlang::is_null(drks_id)){
    rlang::abort("Both `filename` and `drks_id` provided. Supply only one.")
  }

  # Get drks html, depending on filepath or drks_id
  if (!rlang::is_null(drks_id)){
    doc <- fetch_drks(drks_id)

    # If no valid drks record, return early
    if (rlang::is_empty(doc)) {return(NULL)}

  } else {
    doc <- rvest::read_html(filepath)
  }

  out <- list()
  out$drks_id <- get_drks_element(doc, ".drks_id p")
  out$facility_affiliation <- get_drks_labels(doc, "ul.recruitmentLocations")

  # If no facilities, return NULL early
  if (length(out$facility_affiliation) == 1 && is.na(out$facility_affiliation)) return(NULL)

  tibble::as_tibble(out)
}


#' Extract DRKS text element
#'
#' Works for single elements only
#'
#' @param doc DRKS hmtl_document, generally from \code{fetch_drks()}
#' @param element css/xpath to element
#'
#' @return Character string

get_drks_element <- function(doc, element) {
  doc %>%
    rvest::html_elements(xpath=paste(selectr::css_to_xpath(element), "/text()")) %>%
    rvest::html_text(trim = TRUE) %>%
    stringi::stri_remove_empty() %>%
    dplyr::na_if("[---]*")
}

#' Extract DRKS date
#'
#' @param doc DRKS hmtl_document, generally from \code{fetch_drks()}
#' @param element css/xpath to element
#'
#' @return Date

get_drks_date <- function(doc, element) {
  doc %>%
    rvest::html_elements(element) %>%
    rvest::html_text() %>%
    stringr::str_extract("\\d{4}/\\d{2}/\\d{2}") %>%
    lubridate::ymd() %>%

    # "DRKS00003691" and "DRKS00005120" have repeated NAs in completion_date
    na.omit()
}

#' Extract DRKS labels from children of given element
#'
#' @param doc DRKS hmtl_document, generally from \code{fetch_drks()}
#' @param element css/xpath to parent element
#'
#' @return Character vector

get_drks_labels <- function(doc, element){
  doc %>%
    rvest::html_elements(element) %>%
    rvest::html_children() %>%
    rvest::html_text2() %>%
    stringr::str_trim() %>%
    dplyr::na_if("[---]*")
}


#' Extract DRKS elements with labels
#'
#' @param doc DRKS hmtl_document, generally from \code{fetch_drks()}
#' @param element css/xpath to element
#'
#' @return Tibble with `label` and `text` columns

get_drks_labels_elements <- function(doc, element){
  out <-
    doc %>%
    rvest::html_elements(element) %>%
    rvest::html_text2() %>%
    # rvest::html_text(trim = TRUE) %>%
    # stringr::str_squish() %>%
    dplyr::na_if("[---]*")

  # If no elements, return NULL early
  if (length(out) == 1 && is.na(out)) return(NULL)

  out %>%
    stringr::str_split(": ", simplify = TRUE, n = 2) %>%
    tibble::as_tibble(.name_repair = ~c("label", "text")) %>%
    filter(text != "[---]*")
}

#' Extract DRKS elements with labels and hyperlinks
#'
#' @param doc DRKS hmtl_document, generally from \code{fetch_drks()}
#' @param element css/xpath to element
#'
#' @return Tibble with `label`, `text`, and `link` columns

get_drks_labels_elements_links <- function(doc, element){

  out <- rvest::html_elements(doc, element) %>%
    purrr::map_dfr(get_drks_label_element_link)

  # If no elements, return NULL early
  if (rlang::is_empty(out)) return(NULL)

  out
}

#' Extract single DRKS element with labels and hyperlinks
#' Helper function for \code{get_drks_labels_elements_links}
#'
#' @param e single node of element
#'
#' @return List of `label`, `text`, and `link`

get_drks_label_element_link <- function(e){

  out <- list()

  # Get label
  out$label <- e %>%
    rvest::html_elements('label') %>%
    rvest::html_text2() %>%
    stringr::str_remove(":$")

  # If no label, no reference (only drks placeholder: "[---]*"), return NULL early
  if (rlang::is_empty(out$label)) return(NULL)

  # Get link element (with text and link)
  link_element <-  rvest::html_elements(e, 'a')

  # If reference has no link, get element only
  if (rlang::is_empty(link_element)){

    out$text <-
      e %>%
      rvest::html_text2() %>%

      # Remove label
      stringr::str_remove("^.*?: ") %>%

      # Some empty references have a label
      dplyr::na_if("[---]*")

    # If no reference, return NULL early
    if (is.na(out$text)) return(NULL)

    out$link <- NA_character_

    # If reference has link, get element and link
  } else {
    out$text <- rvest::html_text2(link_element)
    out$link <- rvest::html_attr(link_element, 'href')
  }

  out
}


#' Extract DRKS label of given element and text of child element
#'
#' @param doc DRKS hmtl_document, generally from \code{fetch_drks()}
#' @param element css/xpath to parent element
#' @param child_element css/xpath to child element
#'
#' @return Tibble with `label` and `text` columns

get_drks_labels_elements_child <- function(doc, element, child_element){
  doc %>%
    rvest::html_elements(element) %>%
    rvest::html_children() %>%
    purrr::map_dfr(get_drks_label_element_child, child_element)
}


#' Extract single DRKS element with label and text of child element
#' Helper function for \code{get_drks_labels_elements_child}
#'
#' @param e single node of element
#' @param child_element css/xpath to child element
#'
#' @return List of `label` and `text`

get_drks_label_element_child <- function(e, child_e){

  out <- list()

  # Get label
  out$label <- e %>%
    rvest::html_element('label') %>%
    rvest::html_text2()

  # Get child text
  out$text <- e %>%
    rvest::html_element(child_e) %>%
    rvest::html_text2() %>%
    stringr::str_trim() %>%
    dplyr::na_if("[---]*")

  out
}
