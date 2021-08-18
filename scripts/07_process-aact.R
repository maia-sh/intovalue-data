library(dplyr)
library(readr)
library(fs)
library(assertr)
library(ctregistries)

source(here::here("R", "functions", "duration_days.R"))

input_dir <- here::here("data", "raw", "registries", "ctgov")
output_dir <- dir_create(here::here("data", "processed", "registries", "ctgov"))


# Get raw aact data -------------------------------------------------------

studies <- read_csv(path(input_dir, "studies", ext = "csv"))
designs <- read_csv(path(input_dir, "designs", ext = "csv"))
interventions <- read_csv(path(input_dir, "interventions", ext = "csv"))
centers <- read_csv(path(input_dir, "centers", ext = "csv"))
references <- read_csv(path(input_dir, "references", ext = "csv"))
ids <- read_csv(path(input_dir, "ids", ext = "csv"))
resp_parties <- read_csv(path(input_dir, "responsible-parties", ext = "csv"))
officials <- read_csv(path(input_dir, "officials", ext = "csv"))
sponsors <- read_csv(path(input_dir, "sponsors", ext = "csv"))
facilities <- read_csv(path(input_dir, "facilities", ext = "csv"))


# Process lead and facility affiliations ----------------------------------


# Since we don't distinguish between sponsors/official/responsible parties, combine affiliations
# Note: For now, using intovalue umc affiliations, so simply save output

resp_parties <-
  resp_parties %>%
  filter(!is.na(affiliation) | !is.na(organization)) %>%

  # Check that each study has only affiliation OR organization, and then merge
  assert_rows(num_row_NAs, in_set(1), c(affiliation, organization)) %>%

  mutate(affiliation = coalesce(affiliation, organization), .keep = "unused") %>%
  distinct() %>%
  mutate(affiliation_type = "Responsible Party")

officials <-
  officials %>%
  distinct() %>%
  filter(!is.na(affiliation)) %>%
  mutate(affiliation_type = "Study Official")

sponsors <-
  sponsors %>%
  filter(lead_or_collaborator == "lead") %>%
  select(-lead_or_collaborator) %>%

  rename(
    main_sponsor = agency_class,
    affiliation = name
  ) %>%
  assert(is_uniq, nct_id) %>%
  mutate(affiliation_type = "Sponsor")

affiliations <-
  sponsors %>%
  bind_rows(resp_parties, officials) %>%
  distinct() %>%
  select(nct_id, affiliation_type, lead_affiliation = affiliation) %>%
  arrange(nct_id)

write_rds(affiliations, path(output_dir, "ctgov-lead-affiliations", ext = "rds"))

facilities <-
  facilities %>%
  rename(facility_affiliation = name)

write_rds(facilities, path(output_dir, "ctgov-facility-affiliations", ext = "rds"))

# Process studies ---------------------------------------------------------

studies <-
  studies %>%

  rename(
    registration_date = study_first_submitted_date,
    summary_results_date = results_first_submitted_date,
    recruitment_status = overall_status
  ) %>%

  # Parse dates, which are either Month Year, or Month Day Year
  # If no day, default to 1st, like intovalue
  mutate(
    start_date = lubridate::parse_date_time(start_month_year, c("my", "mdY")),
    completion_date = lubridate::parse_date_time(completion_month_year, c("my", "mdY")),
    primary_completion_date = lubridate::parse_date_time(primary_completion_month_year, c("my", "mdY")),
    .keep = "unused"
  ) %>%

  mutate(
    has_summary_results = if_else(!is.na(summary_results_date), TRUE, FALSE)
  ) %>%
  #
  # mutate(
  #   days_reg_to_start = duration_days(registration_date, start_date),
  #   days_reg_to_comp = duration_days(registration_date, completion_date),
  #   days_comp_to_summary = duration_days(completion_date, summary_results_date)
  # ) %>%

  mutate(
    phase = na_if(phase, "N/A"),
    study_type =
      if_else(
        stringr::str_detect(study_type, "Observational"),
        "Observational", study_type
      )
  ) %>%

  # Check that designs.centers have no duplicates per study, and add to studies
  left_join(assertr::assert(designs, is_uniq, nct_id), by = "nct_id") %>%
  left_join(assertr::assert(centers, is_uniq, nct_id), by = "nct_id") %>%
  left_join(select(sponsors, nct_id, main_sponsor), by = "nct_id") %>%

  mutate(
    allocation =  na_if(allocation, "N/A"),
    is_multicentric = !has_single_facility
  ) %>%

  rename(title = brief_title) %>%

  select(-official_title, -has_single_facility, -number_of_facilities)

write_rds(studies, path(output_dir, "ctgov-studies", ext = "rds"))

# Process interventions ---------------------------------------------------

# Note: Unclear how IntoValue selects single intervention_type, so for now disregard
# https://github.com/quest-bih/IntoValue2/blob/msh-dataset-checks/code/Create_CTgov_sample.R#L174 (collapses interventions but iv has single intervention)
# interventions %>%
#   distinct() %>%
#   janitor::get_dupes(nct_id) %>%
#   count(dupe_count)
#
# interventions %>%
#   distinct() %>%
#   group_by(nct_id) %>%
#   mutate(intervention_types = paste(intervention_type, collapse="; ")) %>%
#   distinct(nct_id, intervention_types)

# Process ids -------------------------------------------------------------

ids <-
  ids %>%
  ctregistries::mutate_trn_registry(id_value) %>%

  # Clean trns and collapse EudraCT entries
  mutate(
    raw_trn = trn,
    trn = purrr::map_chr(raw_trn, ctregistries::clean_trn)
  )

write_rds(ids, path(output_dir, "ctgov-ids", ext = "rds"))

crossreg <-
  ids %>%
  filter(!is.na(trn)) %>%
  select(nct_id, crossreg_registry = registry, crossreg_trn = trn)

write_rds(crossreg, path(output_dir, "ctgov-crossreg", ext = "rds"))

# Process references ------------------------------------------------------

references <-
  references %>%

  # Extract identifiers
  mutate(
    pmid = as.numeric(pmid),
    doi = stringr::str_extract(citation, "10\\.\\d{4,9}/[-.;()/:\\w\\d]+"),
    doi = stringr::str_remove(doi, "\\.$"), # remove trailing period
    pmcid = stringr::str_extract(citation, "PMC[0-9]{7}")
  ) %>%

  # Some references are automatically derived in ct.gov
  mutate(reference_derived = if_else(reference_type == "derived", TRUE, FALSE))

write_rds(references, path(output_dir, "ctgov-references", ext = "rds"))
