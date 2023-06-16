# Create codebook based on IntoValue codebook
# Codebook structure created in code and descriptions (based on original intovalue, where available) added in google sheets and read in

library(dplyr)
library(readr)
library(here)
library(stringr)

trials <- read_rds(here("data", "processed", "trials.rds"))


# 1. Prepare codebook structure -------------------------------------------

# Get intovalue codebook for descriptions
iv_codebook <-

  read_csv("https://zenodo.org/record/5141343/files/iv_data_dictionary.csv?download=1") |>

  # Update publication identifier names
  mutate(name = recode(name,
                       publication_doi = "doi",
                       publication_pmid = "pmid",
                       publication_url = "url")
  ) |>

  # Remove publication boolean
  filter(name != "has_publication") |>

  select(name, description)


# Check whether any unused variables in modified intovalue codebook
iv_unused_vars <- setdiff(iv_codebook$name, colnames(trials))
if (length(iv_unused_vars) > 0){
  rlang::warn(glue::glue("Unused IntoValue variables:{iv_unused_vars}"))
}

# Get levels of variables as a string (colon-separated)
get_levels <- function(var){

  trials |>
    filter(!is.na({{var}})) |>
    distinct({{var}}) |>
    arrange({{var}}) |>
    pull() |>
    str_c(collapse = "; ")
}

# Get city levels
lead_cities_levels <-
  trials |>
  distinct(lead_cities) |>
  mutate(lead_cities = str_split(lead_cities, " ")) |>
  tidyr::unnest_longer(lead_cities) |>
  distinct(lead_cities) |>
  filter(!is.na(lead_cities)) |>
  arrange(lead_cities) |>
  pull() |>
  str_c(collapse = "; ")

facility_cities_levels <-
  trials |>
  distinct(facility_cities) |>
  mutate(facility_cities = str_split(facility_cities, " ")) |>
  tidyr::unnest_longer(facility_cities) |>
  distinct(facility_cities) |>
  filter(!is.na(facility_cities)) |>
  arrange(facility_cities) |>
  pull() |>
  str_c(collapse = "; ")

# Get variable types
variable_types <-
  trials |>

  # Change class from glue to character
  mutate(citation = as.character(citation)) |>
  purrr::map_chr(class) |>
  tolower()

# Build codebook structure
codebook_structure <-

  # Add types
  tibble(
    name = colnames(trials),
    type = tolower(variable_types)
  ) |>

  # Add levels
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
    name == "permission_issuer" ~ get_levels(permission_issuer)
  )) |>

  # Add original iv descriptions
  tidylog::left_join(iv_codebook, by = "name")

write_csv(codebook_structure, here("data", "processed", "codebook-structure.csv"))

# `codebook_structure` then manually added and edited in google sheets


# 2. Build codebook with descriptions -------------------------------------
# Retrieve codebook from google sheet unless already retrieved

codebook_path <- here("data", "processed", "codebook.csv")

# If codebook already built, allow user to skip download and build
if (fs::file_exists(codebook_path)){

  codebook <- read_csv(codebook_path, show_col_types = FALSE)

  # Check whether all variables included
  if (identical(codebook_structure$name, codebook$name)){
    rebuild_codebook <- askYesNo(glue::glue(
      "Codebook already available at
      `{fs::path_rel(codebook_path)}`
      Are you sure you want to re-download from Google Sheets and rebuild?",
    ), default = FALSE
    )
  }

  if (!rebuild_codebook){
    stop("Exiting script and not rebuilding codebook", call. = FALSE)
  }
}

# Use google identity (i.e., gmail) to access for google sheets
# Get google identity if locally stored as "google", if available
# Else ask user and store
google_id <-
  ifelse(
    nrow(keyring::key_list("google")) == 1,
    keyring::key_get("google"),
    keyring::key_set("google")
  )

message("Accessing googlesheets via: ", google_id)

# If new google identity, prompt user in web browser to authenticate
googlesheets4::gs4_auth(google_id)

codebook_descriptions <-
  googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1h9MyoeaP4owH4-5ySLob0RMEuud5omKOLMeIY8C4-UE/edit#gid=807502810", sheet = "codebook")

# Combine codebook structure and descriptions
codebook <-
  left_join(
    select(codebook_structure, -description),
    select(codebook_descriptions, name, description, source, is_derived)
  )

# Check that codebook is complete, i.e., no NAs aside from levels
assertr::assert(codebook, assertr::not_na, name, type, description, source, is_derived)

write_csv(codebook_structure, codebook_path)
