library(dplyr)
library(assertr)
library(assertive)
library(pointblank)

trials <- readr::read_rds(here::here("data", "processed", "trials.rds"))
intovalue <- readr::read_csv(here::here("data", "raw", "intovalue.csv"))

# Check that all intovalue columns in trials (except `has_publication`)
if (!rlang::is_empty(setdiff(colnames(intovalue), c(colnames(trials), "has_publication")))) {
  rlang::warn("There are intovalue columns missing from trials!")
}

trials %>%

  # Check that same number of rows as intovalue
  assertr::verify(nrow(.) == nrow(intovalue)) %>%

  # Validate publication dois
  pointblank::col_vals_regex(
    columns = vars(doi),
    regex = "^10\\.\\d{4,9}/[-.;()/:_[:alnum:]]+$",
    na_pass = TRUE
  ) %>%

  # Validate publication pmids
  pointblank::col_vals_regex(
    columns = vars(pmid),
    regex = "^[0-9]{8}$",
    na_pass = TRUE
  ) %>%

  # Validate publication urls
  pointblank::col_vals_regex(
    columns = vars(url),
    regex = "^http",
    na_pass = TRUE
  ) %>%

  # Publication urls should not go to registries
  assertr::assert(
    function(url) !stringr::str_detect(url, "clinicaltrials.gov|drks.de")|is.na(url),
    url
  ) %>%

  # Check that all trials with publication (including abstract) has pmid, doi, OR url
  assertr::verify(nrow(filter(., identification_step != "No publ" & is.na(url) & is.na(doi) & is.na(pmid))) == 0) %>%

  # Check that if no publication, no publication ids
  pointblank::col_vals_null(
    vars(url, doi, pmid),
    preconditions = ~ . %>% filter(identification_step == "No publ")
  ) %>%

  # Check that journal articles have publication_date
  pointblank::col_vals_not_null(
    vars(publication_date),
    preconditions = ~ . %>% filter(publication_type == "journal article")
  )
