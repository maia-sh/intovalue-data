library(dplyr)
library(purrr)
library(fs)
library(readr)
library(stringr)
library(ctregistries)

source(here::here("scripts", "functions", "drks-functions.R"))
source(here::here("scripts", "functions", "duration_days.R"))

input_dir <- here::here("data", "raw", "registries", "drks")
output_dir <- dir_create(here::here("data", "processed", "registries", "drks"))

# Parse all records in directory, alternatively could use drks_trns
drks_htmls <-
  dir_ls(input_dir)

# drks_htmls <- drks_htmls[1:50]

# Parse secondary ids -----------------------------------------------------

drks_ids <-
  drks_htmls %>%
  map_dfr(parse_drks_ids) %>%
  ctregistries::mutate_trn_registry(id) %>%

  # Several ISRCTN are misformatted so capture here
  mutate(

    trn = if_else(
      str_detect(id, "\\d{8}[[:blank:][:punct:]]*ISRCTN"),
      str_c("ISRCTN", str_extract(id, "\\d{8}")),
      trn
    ),

    registry = if_else(
      str_detect(id, "\\d{8}[[:blank:][:punct:]]*ISRCTN"),
      "ISRCTN",
      registry
    )
  ) %>%

  # Clean trns and collapse EudraCT entries
  mutate(
    raw_trn = trn,
    trn = purrr::map_chr(raw_trn, ctregistries::clean_trn),
    id_type = if_else(stringr::str_detect(id_type, "EudraCT"),"EudraCT", id_type)
  )

write_rds(drks_ids, path(output_dir, "drks-ids", ext = "rds"))

drks_crossreg <-
  drks_ids %>%
  filter(!is.na(trn)) %>%
  select(drks_id, crossreg_registry = registry, crossreg_trn = trn)

write_rds(drks_crossreg, path(output_dir, "drks-crossreg", ext = "rds"))

# Parse references --------------------------------------------------------

drks_references <-
  drks_htmls %>%

  map_dfr(parse_drks_references) %>%

  # Extract dois and pmids, from both `citation` and `link`
  # Note: regexes adapted for edge cases as described in comments
  mutate(

    # Some (e.g., DRKS00004841) have >1 doi --> extract all and unnest)
    # One (i.e., DRKS00000005) has has extra space in doi --> extract and clean
    # One (i.e., DRKS00006162) has doi w & wo "." --> ignore duplicate
    # One (i.e., DRKS00010037) has "‑" instead of "-" --> extract and clean
    doi_citation = str_extract_all(citation, "10\\.\\d{4,9}/\\s?[-‑.;()/:\\w\\d]+"),

    # Some (e.g., DRKS00000639, DRKS00000525) have dois with pmid matches surrounded by numbers --> disallow preceding/following numbers
    # Some (e.g., DRKS00005594, DRKS00008054) have  dois with pmid matches preceded by "/" --> disallow preceding "/"
    # Some (e.g., DRKS00000060, DRKS00003174) has pmid within pubmed link --> allow preceding "pubmed/"
    # Some (e.g., DRKS00004097) have old (i.e., from 1990s) 7-digit pmids --> ignore since not trial results publications
    # Some (e.g., DRKS00004675, DRKS00008023) have DRKS id in citation --> disallow preceding "DRKS" on pmid
    # Some (e.g., DRKS00006958) starting with 0 --> disallow starting with 0 in pmid
    # Note: none with >1 pmid but extract all and unnest in case
    pmid_citation = str_extract_all(citation, "(?<!\\d|DRKS|(?<!pubmed)/)[1-9]{1}[0-9]{7}(?!\\d)"),

    pmcid_citation = str_extract(citation, "PMC[0-9]{7}"),

    doi_link = str_extract_all(link, "10\\.\\d{4,9}/\\s?[-‑.;()/:\\w\\d]+"),
    pmid_link = str_extract_all(link, "(?<!\\d|DRKS|(?<!pubmed)/)[1-9]{1}[0-9]{7}(?!\\d)"),
    pmcid_link = str_extract(link, "PMC[0-9]{7}")
  ) %>%
  tidyr::unnest_longer(doi_citation) %>%
  tidyr::unnest_longer(pmid_citation) %>%
  tidyr::unnest_longer(doi_link) %>%
  tidyr::unnest_longer(pmid_link) %>%

  # Clean up doi
  mutate(
    doi_citation = str_replace_all(doi_citation, "\\s", ""), # extra spaces
    doi_citation = str_replace_all(doi_citation, "‑", "-"),  # incorrect dashes
    doi_citation = str_remove(doi_citation, "\\.$"),         # trailing periods
    doi_citation = str_remove(doi_citation, "/epdf$"),        # trailing "/epdf"

    doi_link = str_replace_all(doi_link, "\\s", ""), # extra spaces
    doi_link = str_replace_all(doi_link, "‑", "-"),  # incorrect dashes
    doi_link = str_remove(doi_link, "\\.$"),         # trailing periods
    doi_link = str_remove(doi_link, "/epdf$")        # trailing "/epdf"
  ) %>%

  # Manually select some dois
  # We will coalesce dois, preferring the `doi_citation`, in case of discrepancy
  # However, on manual inspection of discrepancies (n = 2), "DRKS00010133" has correct doi in `doi_link` so manually correct
  # "DRKS00005140" has 2 different dois so prefer `doi_citaton` in coalesce
  # Also capture any discrepancies
  # "DRKS00010133" is missing "z" in `doi_citation`, so replace with `doi_link`
  mutate(
    doi_citation = if_else(drks_id == "DRKS00010133", doi_link, doi_citation)
  ) %>%

  mutate(
    doi = coalesce(doi_citation, doi_link),
    pmid = coalesce(pmid_citation, pmid_link),
    pmid = as.numeric(pmid),
    pmcid = coalesce(pmcid_citation, pmcid_link)
  )

# Inform about discrepancies

doi_discrepancies <-
  drks_references %>%
  filter(!is.na(doi_link) & !is.na(doi_citation)) %>%
  filter(doi_link != doi_citation) %>%
  select(drks_id, doi, doi_citation, doi_link)

if (nrow(doi_discrepancies) != 0){
  rlang::inform(glue::glue("There are DOI discrepancies"))
  rlang::inform(glue::glue("{colnames(doi_discrepancies)}: {doi_discrepancies}"))
}

pmid_discrepancies <-
  drks_references %>%
  filter(!is.na(pmid_link) & !is.na(pmid_citation)) %>%
  filter(pmid_link != pmid_citation) %>%
  select(drks_id, pmid, pmid_citation, pmid_link)

if (nrow(pmid_discrepancies) != 0){
  rlang::inform(glue::glue("There are PMID discrepancies"))
  rlang::inform(glue::glue("{colnames(pmid_discrepancies)}: {pmid_discrepancies}"))
}

pmcid_discrepancies <-
  drks_references %>%
  filter(!is.na(pmcid_link) & !is.na(pmcid_citation)) %>%
  filter(pmcid_link != pmcid_citation) %>%
  select(drks_id, pmcid, pmcid_citation, pmcid_link)

if (nrow(pmcid_discrepancies) != 0){
  rlang::inform(glue::glue("There are PMCID discrepancies"))
  rlang::inform(glue::glue("{colnames(pmcid_discrepancies)}: {pmcid_discrepancies}"))
}


drks_references <-
  drks_references %>%

  # Remove`_citation` and `_link` identifiers
  select(-ends_with("_citation"), -ends_with("_link")) %>%

  # No references are automatically derived in drks
  mutate(reference_derived = FALSE) %>%

  # Infer whether summary results based on reference type and keywords
  # Note: This likely has low accuracy
  mutate(
    is_summary_results = if_else(
      str_detect(citation,
                 "Ergebnisbericht|Abschlussbericht|Studienergebnisse|Studienergebnisbericht|study results")
      & reference_type %in% c("Trial results", "Further trial documents"),
      TRUE, FALSE
    )
  )

write_rds(drks_references, path(output_dir, "drks-references", ext = "rds"))

# Parse studies -----------------------------------------------------------

# IntoValue does not include interventions for drks. Seem to be available in search under "Study Topic" (https://www.drks.de/drks_web/navigate.do?navigationId=search&reset=true) but not in registratration. Not same as "Purpose."

drks_summary_results <-
  drks_references %>%
  filter(is_summary_results) %>%
  pull(drks_id)

drks_studies <-
  drks_htmls %>%
  map_dfr(parse_drks_study) %>%
  mutate(
    phase = na_if(phase, "N/A"),
    enrollment = as.numeric(enrollment),
    main_sponsor = if_else(investigator_initiated == "yes", "Other", "Industry"),
    is_multicentric = if_else(centers == "Multicenter trial", TRUE, FALSE),
    has_summary_results = if_else(drks_id %in% drks_summary_results, TRUE, FALSE),

    # Coalesce study type info and clean observational
    study_type = coalesce(study_type_non_interventional, study_type),
    study_type =
      if_else(
        stringr::str_detect(study_type, "Observational"),
        "Observational", study_type
      )
    # days_reg_to_start = duration_days(registration_date, start_date),
    # days_reg_to_comp = duration_days(registration_date, completion_date),
    # days_comp_to_summary = duration_days(completion_date, summary_results_date)
  ) %>%
  select(-study_type_non_interventional, -masking_who, -purpose,
         -running, -centers, -national, -investigator_initiated)

write_rds(drks_studies, path(output_dir, "drks-studies", ext = "rds"))


# Process lead and facility affiliations ----------------------------------

drks_lead_affiliations <-
  drks_htmls %>%
  map_dfr(parse_drks_affiliations) %>%
  distinct()

write_rds(drks_lead_affiliations, path(output_dir, "drks-lead-affiliations", ext = "rds"))

drks_facility_affiliations <-
  drks_htmls %>%
  map_dfr(parse_drks_facilities) %>%
  distinct()

write_rds(drks_facility_affiliations, path(output_dir, "drks-facility-affiliations", ext = "rds"))
