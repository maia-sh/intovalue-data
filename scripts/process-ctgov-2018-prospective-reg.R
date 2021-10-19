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
dir_ctgov_raw <- path(dir_ctgov, "raw")
dir_ctgov_processed <- path(dir_ctgov, "processed")

# Raw data from NR (2021-09-09)
# One row per NCT-id – UMC pair, so NCTs can appear in multiple rows if they are associated to multiple UMCs. Also there is a city category "All trials combined" which contains each ID once
# AACT dataset pulled on 2029-03-15 and filtered for start dates from 2006 through 2018
# Trials manually screened to confirm UMC affiliation

ctgov_2018 <-
  read_rds(path(dir_main, "CT_gov_delayed_registration_3.rds")) %>%
  janitor::clean_names()

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

# Get unique clinicaltrial.gov trns
ct_trns <- unique(ctgov_2018$id)


# Download and process AACT data ------------------------------------------

# Specify aact username
AACT_USER <- "respmetrics"

download_aact(ids = ct_trns, dir = dir_ctgov_raw, user = AACT_USER)

process_aact(dir_ctgov_raw, dir_ctgov_processed)


# Check AACT studies ------------------------------------------------------

studies <- read_rds(path(dir_ctgov_processed, "ctgov-studies.rds"))

# Check for missing trns
message("There are TRNs not retrieved from AACT: ", setdiff(ct_trns, studies$nct_id))

# There are some trials (n = 22) which now have start dates after 2018
filter(studies, start_date > "2018-12-31")

# There is one trial without a start date
filter(studies, is.na(start_date))


# Prepare trials for prospective registration analysis --------------------

trials_crossreg_eudract <-
  read_rds(path(dir_ctgov_processed, "ctgov-crossreg.rds")) %>%
  filter(crossreg_registry == "EudraCT") %>%
  distinct(id = nct_id) %>%
  pull()

trials <-

  studies %>%

  rename(id = nct_id) %>%

  mutate(
    registry = "ClinicalTrials.gov",

    completion_year = year(completion_date),
    primary_completion_year = year(primary_completion_date),

    # Trials are randomized if allocation includes randomized
    # Otherwise, not randomized (unless no allocation, then NA)
    is_randomized =
      if_else(stringr::str_detect(allocation, "(?i)(?<!non-)randomized"), TRUE, FALSE),

    # Registration is prospective if registered in same or prior month to start
    days_reg_to_start = duration_days(registration_date, start_date),
    is_prospective =
      (floor_date(registration_date, unit = "month") <=
         floor_date(start_date, unit = "month")),

    # Add flag for euctr (potential) cross-registrations
    has_crossreg_eudract = if_else(id %in% trials_crossreg_eudract, TRUE, FALSE)

  ) %>%

  # Use intervention_type from input data
  left_join(distinct(ctgov_2018, id, intervention_type), by = "id") %>%

  # Add cities
  left_join(cities, by = "id") %>%

  # Add intovalue exclusion criteria
  mutate(

    # IntoValue includes all drks and some ctgov recruitment statuses
    iv_status = if_else(
      recruitment_status %in% c("Completed" , "Terminated" , "Suspended", "Unknown status"), TRUE, FALSE
    ),

    # IntoValue includes only interventional studies
    iv_interventional = if_else(study_type == "Interventional", TRUE, FALSE)
  ) %>%

  # Start date should not be missing and be between 2006 and 2018
  mutate(
    start_2006_2018 = if_else(!is.na(start_date) & start_date < "2018-12-31" & start_date > "2006-01-01", TRUE, FALSE)
  ) %>%

  select(-last_update_submitted_date)

# Apply exclusion criteria per decision on 2021-10-08
trials %>%

  filter(start_2006_2018 & iv_interventional & iv_status) %>%

  write_csv(here(dir_main, "prospective-reg-ctgov-2018-trials.csv"))
