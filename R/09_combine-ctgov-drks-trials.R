library(dplyr)
library(readr)
library(fs)
library(lubridate)

source(here::here("R", "functions", "duration_days.R"))

# Get data ----------------------------------------------------------------

ctgov_dir <- here::here("data", "processed", "ctgov")
drks_dir <- here::here("data", "processed", "drks")

ctgov_studies <- read_rds(path(ctgov_dir, "ctgov-studies.rds"))
ctgov_references <- read_rds(path(ctgov_dir, "ctgov-references.rds"))

drks_studies <- read_rds(path(drks_dir, "drks-studies.rds"))
drks_references <- read_rds(path(drks_dir, "drks-references.rds"))


# Combine registry studies ------------------------------------------------

ctgov_studies <-
  ctgov_studies %>%
  rename(id = nct_id) %>%
  select(-last_update_submitted_date) %>%
  mutate(registry = "ClinicalTrials.gov")

drks_studies <-
  drks_studies %>%
  rename(id = drks_id) %>%
  mutate(registry = "DRKS")

# Check registry column names:
# Expect none in drks not in ctgov
# Expect pcd and summary results date in ctgov and not in drks
if (
  !rlang::is_empty(setdiff(colnames(drks_studies), colnames(ctgov_studies))) |
  rlang::is_empty(setdiff(colnames(ctgov_studies), colnames(drks_studies))) |
  !all(setdiff(colnames(ctgov_studies), colnames(drks_studies)) %in% c("summary_results_date", "primary_completion_date"))
) {rlang::warn("There are unexpected drks and/or ctgov column names!")}

registry_studies <-
  bind_rows(ctgov_studies, drks_studies) %>%


  mutate(

    # Trials from registries are resolved
    is_resolved = TRUE,

    # Trials are randomized if allocation includes randomized
    # Otherwise, not randomized (unless no allocation, then NA)
    is_randomized =
      if_else(stringr::str_detect(allocation, "(?i)(?<!non-)randomized"), TRUE, FALSE),

    # Registration is prospective if registered in same or prior month to start
    is_prospective =
      (floor_date(registration_date, unit = "month") <=
      floor_date(start_date, unit = "month")),

    # Update registry date variables to match intovalue
    completion_year = year(completion_date),
    primary_completion_year = year(primary_completion_date),
    days_cd_to_summary = duration_days(completion_date, summary_results_date),
    days_pcd_to_summary = duration_days(primary_completion_date, summary_results_date),
    days_reg_to_start = duration_days(registration_date, start_date),
    days_reg_to_cd = duration_days(registration_date, completion_date),
    days_reg_to_pcd = duration_days(registration_date, primary_completion_date)
  )

write_rds(registry_studies, here::here("data", "processed", "registry-studies.rds"))

# Combine registry references ---------------------------------------------

drks_references <-
  drks_references %>%
  select(
    id = drks_id,
    doi,
    pmid,
    reference_type,
    reference_derived
  )

ctgov_references <-
  ctgov_references %>%
  select(
    id = nct_id,
    doi,
    pmid,
    reference_type,
    reference_derived
  )

registry_references <-
  bind_rows(ctgov_references, drks_references) #%>%
  # filter(!(is.na(doi) & is.na(pmid))) %>%
  # distinct()

write_rds(registry_references, here::here("data", "processed", "registry-references.rds"))
