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

# drks_id <- "DRKS00000014"
# doc <- fetch_drks("DRKS00004841")
fetch_drks <- function(drks_id, quiet = FALSE) {

  drks_url <- "https://drks.de/search/en/trial/"

  doc <-
    rvest::read_html(glue::glue("{drks_url}{drks_id}"))

  doc_id <- get_drks_org_data(doc)$`DRKS-ID`

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

parse_drks_study <- function(filepath = NULL, drks_id = NULL) {

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

  drks_org_data <- get_drks_org_data(doc)
  drks_recruitment <- get_drks_recruitment(doc)
  drks_study_design <- get_drks_study_design(doc)

  out <- list()
  out$drks_id <-  drks_org_data$`DRKS-ID`
  out$title <- get_drks_element(doc, "h3")[1]
  out$registration_date <- drks_org_data$Date_of_registration_in_DRKS
  # out$start_date <- get_drks_date(doc, "div.col-md-6.mb-3 div.col-sm-6")
  out$start_date <- drks_recruitment$Actual_study_start_date
  # out$completion_date <- get_drks_date(doc, ".deadline")
  out$completion_date <- drks_recruitment$Actual_Study_Completion_Date
  out$allocation <- drks_study_design$Allocation
  out$study_type <- drks_study_design$Study_type
  # out$study_type_non_interventional <- get_drks_element(doc, ".typeNotInterventional")
  out$masking <- drks_study_design$Blinding
  out$masking_who <- drks_study_design$Who_is_blinded
  out$phase <- drks_study_design$Phase
  out$purpose <- drks_study_design$Purpose
  # what is running
  # out$running <- get_drks_element(doc, ".running")
  #target vs final sample size
  out$enrollment <- drks_recruitment$Final_Sample_Size
  out$centers <- drks_recruitment$Number_of_study_centers
  # out$national <- get_drks_element(doc, "div.col-md-6.mb-3 div.card.h-100 dd")[1] national vs list of countries
  out$national <- drks_recruitment$Recruitment_countries # national vs list of countries
  out$recruitment_status <- drks_org_data$Recruitment_Status
  out$investigator_initiated <- get_drks_element(doc, "div.card.mb-4:nth-child(10) dd:last-child span.withLineBreak")

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

  id_type <- get_drks_element(doc, "div.mb-3.col-md-6.mb-0 dt")
  id <- get_drks_element(doc, "div.mb-3.col-md-6.mb-0 dd")

  # If no ids, return NULL early
  if (rlang::is_null(id_type)) return(NULL)

  tibble::tibble(id_type, id) |>
    dplyr::filter(!is.na(id))
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

  out <- get_drks_labels_elements_links(doc, "#studyResults ~ div.card-body")
  out <- dplyr::bind_rows(out, get_drks_labels_elements_links(doc, "#studyProtocols ~ div.card-body"))

  out <- out |>
    dplyr::filter(!(is.na(text) & is.na(link)),
                  !stringr::str_detect(text, "^\\d{4}-\\d{2}-\\d{2}"))

  # If no ids, return NULL early
  if (rlang::is_null(out)) return(NULL)

  out |>
    dplyr::rename(reference_type = label, citation = text)  |>
    dplyr::mutate(drks_id = get_drks_org_data(doc)$`DRKS-ID`, .before = 1)
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

    labels <- get_drks_element(doc, "div.col-md.mb-3 h4")
    # If no affiliations, return NULL early
    if (rlang::is_null(labels)) return(NULL)
    labels <- labels[!stringr::str_detect(labels, "Vote")]

    institutions <- get_drks_element(doc, "div.col-md.mb-3 div.card-body dd div:first-child")

  out <- tibble::tibble(affiliatoin_type = labels, lead_affiliation = institutions)

  out |>
    dplyr::mutate(drks_id =  get_drks_org_data(doc)$`DRKS-ID`, .before = 1)

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
  out$drks_id <- get_drks_org_data(doc)$`DRKS-ID`
  out$facility_affiliation <- get_drks_recruitment(doc)$`Recruitment_location(s)`

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
  doc |>
    rvest::html_elements(xpath=paste(selectr::css_to_xpath(element)))  |>
    rvest::html_text(trim = TRUE) |>
    stringi::stri_remove_empty() |>
    dplyr::na_if("No Entry")
}

#' Extract DRKS Organisational Data
#'
#' @param doc DRKS html_document, generally from \code{fetch_drks()}
#'
#' @return Named list

get_drks_org_data <- function(doc) {
  get_drks_element(doc, "div.card.trial-details-float.mb-4") |>
    string_to_named_list()
}

#' Extract DRKS Recruitment Locations and Period and Number of Participants
#'
#' @param doc DRKS html_document, generally from \code{fetch_drks()}
#'
#' @return Named list

get_drks_recruitment <- function(doc) {
  get_drks_element(doc, "div.col-md-6.mb-3 div.card.h-100 div.card-body") |>
    string_to_named_list()
}


#' Extract DRKS Study design
#'
#' @param doc DRKS html_document, generally from \code{fetch_drks()}
#'
#' @return Named list

get_drks_study_design <- function(doc) {
  get_drks_element(doc, "div.mb-3.col-md-4.mb-0") |>
    string_to_named_list()
}

# str_in <- get_drks_element(doc, "div.col-md-6.mb-3 div.col-sm-6")
string_to_named_list <- function(str_in, remove_title = TRUE) {
  vec <- str_in |>
    stringr::str_remove_all("\t") |>
    stringr::str_remove_all("(?<=\n)\n") |>
    stringr::str_split("\n") |>
    unlist()
  if (remove_title == TRUE) {
    if (!stringr::str_detect(vec[1], ":")) vec <- vec[-1]
  }

  vec_names <- vec[stringr::str_detect(vec, ":")] |>
    stringr::str_remove(":") |>
    stringr::str_replace_all(" ", "_")
  vec_values <- vec[!stringr::str_detect(vec, ":")] |>
    dplyr::na_if("No Entry")

  out <- list()
  for (i in 1:length(vec_names)) {
    out[[vec_names[i]]] <- vec_values[i]
  }
 out
}

#' Extract DRKS date
#'
#' @param doc DRKS hmtl_document, generally from \code{fetch_drks()}
#' @param element css/xpath to element
#'
#' @return Date

# get_drks_date <- function(doc, element) {
#   doc %>%
#     rvest::html_elements(element) %>%
#     rvest::html_text() %>%
#     stringr::str_extract("\\d{4}/\\d{2}/\\d{2}") %>%
#     lubridate::ymd() %>%
#
#     # "DRKS00003691" and "DRKS00005120" have repeated NAs in completion_date
#     na.omit()
# }

#' Extract DRKS labels from children of given element
#'
#' @param doc DRKS hmtl_document, generally from \code{fetch_drks()}
#' @param element css/xpath to parent element
#'
#' @return Character vector
#
# get_drks_labels <- function(doc, element){
#   doc %>%
#     rvest::html_elements(element) %>%
#     rvest::html_children() %>%
#     rvest::html_text2() %>%
#     stringr::str_trim() %>%
#     dplyr::na_if("[---]*")
# }


#' Extract all DRKS elements with labels
#'
#' @param doc DRKS hmtl_document, generally from \code{fetch_drks()}
#'
#' @return Tibble with `label` and `text` columns
#
# get_drks_id_tib <- function(doc){
#   labels <-
#     doc |>
#     rvest::html_elements("div.card-body  dl  dt") |>
#     rvest::html_text2()
#
#   labels <- tibble::tibble(label = labels) |>
#     filter(stringr::str_detect(label, ":"))
#
#   # If no elements, return NULL early
#   if (nrow(labels) == 1) return(NULL)
#
#   text <-
#     doc |>
#     rvest::html_elements("div.card-body > dl > dd") |>
#     rvest::html_text2()
#
#   text <- tibble::tibble(text = text) |>
#     filter(!stringr::str_detect(text, ":$"))
#
#   bind_cols(labels, text)
#
# }


#' Extract DRKS elements with labels
#'
#' @param doc DRKS hmtl_document, generally from \code{fetch_drks()}
#' @param element css/xpath to element
#'
#' @return Tibble with `label` and `text` columns
# element <- "#studyProtocols ~ div.card-body"
# element <- "div.col-md-6.mb-3 div.card.h-100"
# get_drks_labels_elements <- function(doc, element, nested = FALSE){
#
#   if (nested == FALSE) {
#     labels <- doc |>
#       rvest::html_elements(paste(element, "dt")) |>
#       rvest::html_text2()
#     # If no elements, return NULL early
#     if (length(labels) == 1 && is.na(labels)) return(NULL)
#
#     texts <- doc |>
#       rvest::html_elements(paste(element, "dd")) |>
#       rvest::html_text2()
#
#     out <- dplyr::bind_cols(label = labels, text = texts) |>
#       dplyr::mutate(text = dplyr::na_if(text, "No Entry"))
#     return( )
#
#   } else {
#
#   }
#   out <-
#
#     # rvest::html_text(trim = TRUE) %>%
#     # stringr::str_squish() %>%
#     # dplyr::na_if("No Entry")
#
#   # If no elements, return NULL early
#   if (length(labels) == 1 && is.na(labels)) return(NULL)
#
#   out |>
#     stringr::str_split("\n", simplify = TRUE) |>
#     tibble::as_tibble(.name_repair = ~c("label", "text")) %>%
#     filter(text != "[---]*")
# }

#' Extract DRKS elements with labels and hyperlinks
#'
#' @param doc DRKS hmtl_document, generally from \code{fetch_drks()}
#' @param element css/xpath to element
#'
#' @return Tibble with `label`, `text`, and `link` columns
element <- "#studyResults ~ div.card-body"
element <- "#studyProtocols ~ div.card-body"
get_drks_labels_elements_links(doc, element)
get_drks_labels_elements_links <- function(doc, element){

  labels <- rvest::html_elements(doc, element) |>
    rvest::html_elements("dt") |>
    rvest::html_text2() |>
    stringr::str_remove_all(":")

  # text_links <-
  out <- rvest::html_elements(doc, element) |>
    rvest::html_elements("dd") |>
    purrr::map(get_drks_element_link) |>
    purrr::map(tibble::as_tibble) |>
    purrr::list_rbind()

  # If no elements, return NULL early
  if (rlang::is_empty(out)) return(NULL)

  dplyr::bind_cols(label = labels, out)
}

#' Extract single DRKS element with hyperlinks
#' Helper function for \code{get_drks_labels_elements_links}
#'
#' @param e single node of element
#'
#' @return List of `text`, and `link`

get_drks_element_link <- function(e) {

  out <- list()
  # e <- doc |>
  #   rvest::html_elements("#studyResults ~ div.card-body dd")

  out$text <-
    e |>
    rvest::html_text2() |>
    dplyr::na_if("No Entry")

  # If no text, no reference, return NULL early
#
#   if (rlang::is_empty(out$text) | is.na(out$text)) return(NULL)

  # Get link element (with text and link)
  link_element <- e |>
    rvest::html_element("a")

  # If reference has no link, get element only
  if (rlang::is_empty(link_element)) {

      # Remove label
      # stringr::str_remove("^.*?: ") |>

      # Some empty references have a label
      # dplyr::na_if("[---]*")

    # If no reference, return NULL early
    # if (is.na(out$text)) return(NULL)

    out$link <- NA_character_

    # If reference has link, get element and link
  } else {
    # out$text <- rvest::html_text2(link_element)
    out$link <- rvest::html_attr(link_element, "href")
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
# element <-
# child_element <-
# get_drks_labels_elements_child <- function(doc, element, child_element){
#   doc |>
#     rvest::html_elements(element)  |>
#     # rvest::html_children()  |>
#     rvest::html_text2() |>
#     purrr::map(get_drks_label_element_child, child_element) |>
#     purrr::list_rbind()
# }


#' Extract single DRKS element with label and text of child element
#' Helper function for \code{get_drks_labels_elements_child}
#'
#' @param e single node of element
#' @param child_element css/xpath to child element
#'
#' @return List of `label` and `text`

# get_drks_label_element_child <- function(e, child_e){
#
#   out <- list()
#
#   # Get label
#   out$label <- e %>%
#     rvest::html_element('label') %>%
#     rvest::html_text2()
#
#   # Get child text
#   out$text <- e %>%
#     rvest::html_element(child_e) %>%
#     rvest::html_text2() %>%
#     stringr::str_trim() %>%
#     dplyr::na_if("[---]*")
#
#   out
# }
