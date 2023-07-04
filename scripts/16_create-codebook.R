# Create codebook based on IntoValue codebook
#
# 1. Check whether the codebook already exists with same variables and, if so, allow the user to skip codebook creation.
# 2. Create the codebook structure using the trials data as well as `definition` from the original intovalue codebook, where available, and save `codebook-structure.csv`.
# 3. Manually upload `codebook-structure.csv` to google spreadsheet to collaboratively add and edit `definition`, as well as add each variable's `source` and whether it `is_derived`.
# 4. Download the manually-edited codebook and use `description`, `source`, and `is_derived` from this new version.

library(dplyr)
library(readr)
library(here)
library(stringr)

trials <- read_rds(here("data", "processed", "trials.rds"))

codebook_structure_path <- here("data", "processed", "codebook-structure.csv")
codebook_path <- here("data", "processed", "codebook.csv")


# 1. Determine whether to build codebook ----------------------------------
# Build codebook if (1) codebook does not yet exist, OR (2) codebook exists but different variables, OR (3) codebook exists and user specifies to rebuild

if (!fs::file_exists(codebook_path)){
  create_codebook <- TRUE
} else {
  codebook <- read_csv(codebook_path, show_col_types = FALSE)

  if (!identical(colnames(trials), codebook$name)) {
    create_codebook <- TRUE
  } else {
    rebuild_codebook <- utils::menu(c("yes", "no"), title = glue::glue("Codebook already available at `{fs::path_rel(codebook_path)}`\nWould you like to rebuild the codebook? (default = no)"))
    if (rebuild_codebook == 1) {
      create_codebook <- TRUE
    } else {
      message("Exiting script and not rebuilding codebook")
      create_codebook <- FALSE
    }
  }
}


# START CODEBOOK CREATION
if (create_codebook){

# 2. Prepare codebook structure with original intovalue definitions -------

# Get intovalue codebook for descriptions
iv_codebook <-

  read_csv("https://zenodo.org/record/5141343/files/iv_data_dictionary.csv?download=1", show_col_types = FALSE) |>

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
  left_join(iv_codebook, by = "name")

write_csv(codebook_structure, codebook_structure_path)


# 3. Manually edit codebook -----------------------------------------------
# Manually upload `codebook-structure.csv` to google spreadsheet to collaboratively add and edit `definition`, as well as add each variable's `source` and whether it `is_derived`.


# 4. Build codebook with descriptions -------------------------------------

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

write_csv(codebook, codebook_path)

} # END CODEBOOK CREATION
