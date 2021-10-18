library(dplyr)
library(dbplyr)
library(DBI)
library(RPostgreSQL)

source(here::here("scripts", "functions", "aact-functions.R"))

dir <- fs::dir_create(here::here("data", "raw", "registries", "ctgov"))

# Get clinicaltrials.gov data via https://aact.ctti-clinicaltrials.org/
# aact is a relational database of clinicaltrials.gov
# some variables of interest (such as registration_date and completion_date) not in intovalue
# also fetch additional ids and publications
# also fetch other dates since possibly changed and for intovalue 1/2 parity
# do no re-fetch variables that are either less likely to change or unclear how to clean like intovalue, e.g.: allocation (shouldn't change), main_sponsor (unclear source), is_multicentric (unclear source), lead_cities (unclear source and lots of intovalue munging)

# Get clinicaltrial.gov trns
ct_trns <-
  readr::read_rds(here::here("data", "processed", "trn", "trn-all.rds")) %>%
  filter(registry == "ClinicalTrials.gov") %>%
  pull(trn)

# ct_trns <- ct_trns[1:5]

# Connect aact database
AACT_USER <- "respmetrics"
con <- connect_aact(AACT_USER)

# Query aact database and write output to files
studies <- query_aact(
  "studies", ct_trns,
  con, filepath = fs::path(dir, "studies", ext = "csv"),
  last_update_submitted_date,
  start_month_year, completion_month_year, primary_completion_month_year,
  study_first_submitted_date, results_first_submitted_date,
  study_type, phase, enrollment, overall_status,
  official_title, brief_title
)

designs <- query_aact(
  "designs", ct_trns,
  con, filepath = fs::path(dir, "designs", ext = "csv"),
  allocation, masking
)

interventions <- query_aact(
  "interventions", ct_trns,
  con, filepath = fs::path(dir, "interventions", ext = "csv"),
  intervention_type
)

references <- query_aact(
  "study_references", ct_trns,
  con, filepath = fs::path(dir, "references", ext = "csv"),
  pmid, reference_type, citation
)

ids <- query_aact(
  "id_information", ct_trns,
  con, filepath = fs::path(dir, "ids", ext = "csv"),
  id_type, id_value
)

centers <- query_aact(
  "calculated_values", ct_trns,
  con, filepath = fs::path(dir, "centers", ext = "csv"),
  has_single_facility, number_of_facilities
)

officials <- query_aact(
  "overall_officials", ct_trns,
  con, filepath = fs::path(dir, "officials", ext = "csv"),
  affiliation
)

responsible_parties <- query_aact(
  "responsible_parties", ct_trns,
  con, filepath = fs::path(dir, "responsible-parties", ext = "csv"),
  affiliation, organization
)

sponsors <- query_aact(
  "sponsors", ct_trns,
  con, filepath = fs::path(dir, "sponsors", ext = "csv"),
  agency_class, # main_sponsor
  lead_or_collaborator, name
)

facilities <- query_aact(
  "facilities", ct_trns,
  con, filepath = fs::path(dir, "facilities", ext = "csv"),
  name, city, country
)

# Disconnect aact database
RPostgreSQL::dbDisconnect(con)

# Log query date
loggit::set_logfile(here::here("queries.log"))
loggit::loggit("INFO", "AACT")
