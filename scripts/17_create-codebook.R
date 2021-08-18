# Create codebook based on IntoValue codebook

library(dplyr)
library(readr)
library(here)
library(stringr)

trials <- read_rds(here("data", "processed", "trials.rds"))

# Get intovalue codebook for descriptions
iv_codebook <-

  read_csv("https://zenodo.org/record/5141343/files/iv_data_dictionary.csv?download=1") %>%

  # Update publication identifier names
  mutate(name = recode(name,
                       publication_doi = "doi",
                       publication_pmid = "pmid",
                       publication_url = "url")
  ) %>%

  # Remove publication boolean
  filter(name != "has_publication") %>%

  select(name, description)


# Check whether any unused variables in modified intovalue codebook
iv_unused_vars <- setdiff(iv_codebook$name, colnames(trials))
if (length(iv_unused_vars) > 0){
  rlang::warn(glue::glue("Unused IntoValue variables:{iv_unused_vars}"))
}

# Get levels of variables as a string (colon-separated)
get_levels <- function(var){

  trials %>%
    filter(!is.na({{var}})) %>%
    distinct({{var}}) %>%
    arrange({{var}}) %>%
    pull() %>%
    str_c(collapse = "; ")
}

# Get city levels
lead_cities_levels <-
  trials %>%
  distinct(lead_cities) %>%
  mutate(lead_cities = str_split(lead_cities, " ")) %>%
  tidyr::unnest_longer(lead_cities) %>%
  distinct(lead_cities) %>%
  filter(!is.na(lead_cities)) %>%
  arrange(lead_cities) %>%
  pull() %>%
  str_c(collapse = "; ")

facility_cities_levels <-
  trials %>%
  distinct(facility_cities) %>%
  mutate(facility_cities = str_split(facility_cities, " ")) %>%
  tidyr::unnest_longer(facility_cities) %>%
  distinct(facility_cities) %>%
  filter(!is.na(facility_cities)) %>%
  arrange(facility_cities) %>%
  pull() %>%
  str_c(collapse = "; ")

# Get new columns and limit trials to new columns
# trials_new_vars <-
#   trials %>%
#   select(-all_of(iv_codebook$name))

# Build codebook for new variables
codebook <-

  tibble(
    name = colnames(trials),
    type = tolower(purrr::map_chr(trials, class))
  ) %>%

  mutate(levels = case_when(
    name == "registry" ~ get_levels(registry),
    name == "identification_step" ~ get_levels(identification_step),
    name == "publication_type" ~ get_levels(publication_type),
    name == "recruitment_status" ~ get_levels(recruitment_status),
    name == "phase" ~ get_levels(phase),
    name == "main_sponsor" ~ get_levels(main_sponsor),
    name == "allocation" ~ get_levels(allocation),
    name == "masking" ~ get_levels(masking),
    name == "intervention_type" ~ get_levels(intervention_type),
    name == "center_size" ~ get_levels(center_size),
    name == "iv_version" ~ get_levels(iv_version),
    name == "lead_cities" ~ lead_cities_levels,
    name == "facility_cities" ~ facility_cities_levels,
    name == "study_type" ~ get_levels(study_type),
    name == "ft_source" ~ get_levels(ft_source),
    name == "color_green_only" ~ get_levels(color_green_only),
    name == "color" ~ get_levels(color),
    name == "syp_response" ~ get_levels(syp_response),
    name == "archiving_locations" ~ get_levels(archiving_locations),
    name == "versions" ~ get_levels(versions),
    name == "licenses_required" ~ get_levels(licenses_required),
    name == "permission_issuer " ~ get_levels(permission_issuer )
  ))

# Prepare variable descriptions for new variables
# cat(colnames(trials_new_vars), sep = '",\n"",\n\n"')
description <- tribble(
  ~name, ~description,

  "is_resolved",
  "",

  "iv_completion",
  "",

  "iv_status",
  "",

  "iv_interventional",
  "",

  "title",
  "",

  "study_type",
  "",

  "results_search_start_date",
  "",

  "results_search_end_date",
  "",

  "has_pubmed",
  "",

  "has_ft",
  "",

  "ft_source",
  "",

  "pub_title",
  "",

  "ppub_date",
  "",

  "epub_date",
  "",

  "has_iv_trn_abstract",
  "",

  "has_iv_trn_secondary_id",
  "",

  "has_iv_trn_ft",
  "",

  "has_reg_pub_link",
  "",

  "pmid_link",
  "",

  "doi_link",
  "",

  "reference_derived",
  "",

  "has_crossreg_clinicaltrials.gov",
  "",

  "has_crossreg_isrctn",
  "",

  "has_crossreg_eudract",
  "",

  "has_crossreg_drks",
  "",

  "has_crossreg_anzctr",
  "",

  "has_crossreg_ntr",
  "",

  "n_crossreg_secondary_id",
  "",

  "n_crossreg_abstract",
  "",

  "n_crossreg_ft",
  "",

  "n_crossreg_reg",
  "",

  "n_reg_pub_any",
  "",

  "n_reg_pub_doi_or_pmid",
  "",

  "results_followup",
  "",

  "has_followup_2y",
  "",

  "has_followup_5y",
  "",

  "is_summary_results_1y",
  "",

  "is_summary_results_2y",
  "",

  "is_summary_results_5y",
  "",

  "is_publication_2y",
  "",

  "is_publication_5y",
  "",

  "color_green_only",
  "",

  "color",
  "",

  "issn",
  "",

  "journal",
  "",

  "publisher",
  "",

  "publication_date_unpaywall",
  "",

  "syp_response",
  "",

  "can_archive",
  "",

  "archiving_locations",
  "",

  "inst_repository",
  "",

  "versions",
  "",

  "submitted_version",
  "",

  "accepted_version",
  "",

  "published_version",
  "",

  "licenses_required",
  "",

  "permission_issuer",
  "",

  "embargo",
  "",

  "date_embargo_elapsed",
  "",

  "is_embargo_elapsed",
  "",

  "permission_accepted",
  "",

  "permission_published",
  "",

  "is_oa",
  "",

  "is_archivable",
  "",

  "is_closed_archivable",
  ""
) %>%

  # Add intovalue descriptions
  bind_rows(iv_codebook)

# Check that all described variables are in the dataset and vice-versa
if (!all(colnames(trials) %in% description$name)| !all(description$name %in% colnames(trials))){
  rlang::warn("There are missing or additional variables for codebook!")
}

codebook <-
  codebook %>%
  left_join(description, by = "name") %>%
  relocate(description, .after = "type")

write_csv(codebook, here("data", "processed", "codebook.csv"))
