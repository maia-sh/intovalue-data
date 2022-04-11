# Get clinicaltrials.gov data via https://aact.ctti-clinicaltrials.org/
# aact is a relational database of clinicaltrials.gov
# some variables of interest (such as registration_date and completion_date) not in intovalue
# also fetch additional ids and publications
# also fetch other dates since possibly changed and for intovalue 1/2 parity
# do no re-fetch variables that are either less likely to change or unclear how to clean like intovalue, e.g.: allocation (shouldn't change), main_sponsor (unclear source), is_multicentric (unclear source), lead_cities (unclear source and lots of intovalue munging)

library(dplyr)
library(aactr)

dir_raw <- here::here("data", "raw", "registries", "ctgov")
dir_processed <- here::here("data", "processed", "registries", "ctgov")

# Get clinicaltrial.gov trns
ct_trns <-
  readr::read_rds(here::here("data", "processed", "trn", "trn-all.rds")) %>%
  filter(registry == "ClinicalTrials.gov") %>%
  pull(trn)

# Specify aact username
AACT_USER <- "respmetrics"

download_aact(ids = ct_trns, dir = dir_raw, user = AACT_USER)

process_aact(dir_raw, dir_processed)
