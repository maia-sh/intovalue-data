library(dplyr)
library(tidyr)
library(stringr)
library(readr)
library(here)
library(fs)
library(lubridate)

source(here::here("scripts", "functions", "aact-functions.R"))
source(here::here("scripts", "functions", "duration_days.R"))

dir_main <- here("data", "ctgov-2018")
dir_ctgov <- path(dir_main, "ctgov")
fs::dir_create(dir_ctgov)

ctgov_2018 <- read_rds(path(dir_main, "CT_gov_delayed_registration_3.rds"))

# Check that all all ctgov trials
if (nrow(filter(ctgov_2018, str_detect(id, "^NCT", negate = TRUE))) != 0){stop("There are some non-ctgov trials!")}

# Prepare cities
# Input data has one row per trial, per UMC, plus "All trials combined"
# NCT03563677 has many UMCs
cities <-
  ctgov_2018 %>%
  filter(city != "All trials combined") %>%
  select(id, city) %>%
  group_by(id) %>%
  mutate(cities = str_c(city, collapse = " ")) %>%
  ungroup() %>%
  select(-city) %>%
  distinct()

# Query AACT --------------------------------------------------------------
# Note: We only need `studies` but run all queries to have raw data

# Get unique clinicaltrial.gov trns
ct_trns <- unique(ctgov_2018$id)

# Connect aact database
AACT_USER <- "respmetrics"
con <- connect_aact(AACT_USER)

# Query aact database and write output to files
studies <- query_aact(
  "studies", ct_trns,
  con, filepath = fs::path(dir_ctgov, "studies", ext = "csv"),
  last_update_submitted_date,
  start_month_year, completion_month_year, primary_completion_month_year,
  study_first_submitted_date, results_first_submitted_date,
  study_type, phase, enrollment, overall_status,
  official_title, brief_title
)

designs <- query_aact(
  "designs", ct_trns,
  con, filepath = fs::path(dir_ctgov, "designs", ext = "csv"),
  allocation, masking
)

interventions <- query_aact(
  "interventions", ct_trns,
  con, filepath = fs::path(dir_ctgov, "interventions", ext = "csv"),
  intervention_type
)

references <- query_aact(
  "study_references", ct_trns,
  con, filepath = fs::path(dir_ctgov, "references", ext = "csv"),
  pmid, reference_type, citation
)

ids <- query_aact(
  "id_information", ct_trns,
  con, filepath = fs::path(dir_ctgov, "ids", ext = "csv"),
  id_type, id_value
)

centers <- query_aact(
  "calculated_values", ct_trns,
  con, filepath = fs::path(dir_ctgov, "centers", ext = "csv"),
  has_single_facility, number_of_facilities
)

officials <- query_aact(
  "overall_officials", ct_trns,
  con, filepath = fs::path(dir_ctgov, "officials", ext = "csv"),
  affiliation
)

responsible_parties <- query_aact(
  "responsible_parties", ct_trns,
  con, filepath = fs::path(dir_ctgov, "responsible-parties", ext = "csv"),
  affiliation, organization
)

sponsors <- query_aact(
  "sponsors", ct_trns,
  con, filepath = fs::path(dir_ctgov, "sponsors", ext = "csv"),
  agency_class, # main_sponsor
  lead_or_collaborator, name
)

facilities <- query_aact(
  "facilities", ct_trns,
  con, filepath = fs::path(dir_ctgov, "facilities", ext = "csv"),
  name, city, country
)

# Disconnect aact database
RPostgreSQL::dbDisconnect(con)

# Log query date
loggit::set_logfile(here::here("queries.log"))
loggit::loggit("INFO", "AACT_ctgov_2018")


# Process AACT ------------------------------------------------------------

studies <- read_csv(path(dir_ctgov, "studies", ext = "csv"))

trials <-
  studies %>%

  rename(
    id = nct_id,
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
  # left_join(assertr::assert(designs, is_uniq, nct_id), by = "nct_id") %>%
  # left_join(assertr::assert(centers, is_uniq, nct_id), by = "nct_id") %>%
  # left_join(select(sponsors, nct_id, main_sponsor), by = "nct_id") %>%

  # mutate(
  #   allocation =  na_if(allocation, "N/A"),
  #   is_multicentric = !has_single_facility
  # ) %>%

  rename(title = brief_title) %>%

  select(
    -official_title,
    # -has_single_facility, -number_of_facilities
  ) %>%

  # Add in cities
  left_join(cities, by = "id")

# Check for missing trns
message("There are TRNs not retrieved from AACT: ", setdiff(ct_trns, studies$nct_id))

# There are some trials (n = 22) which now have start dates after 2018
trials %>%
  filter(start_date > "2018-12-31")

# There is one trial without a start date
trials %>%
  filter(is.na(start_date))


# Prepare trials for prospective registration analysis --------------------

prospective_reg_trials <-
  trials %>%

  mutate(
    days_reg_to_start = duration_days(registration_date, start_date),
    is_prospective =
      (floor_date(registration_date, unit = "month") <=
         floor_date(start_date, unit = "month"))
  ) %>%

  # Exclude trials with start date missing or after 2018
  filter(
    start_date < "2018-12-31",
    !is.na(start_date)
  ) %>%

  select(
    id,
    cities,
    registration_date,
    start_date,
    days_reg_to_start,
    is_prospective
  )


write_csv(prospective_reg_trials, here(dir_main, "prospective-reg-ctgov-2018-trials.csv"))



