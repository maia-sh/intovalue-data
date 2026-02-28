## Script to compile all TRNs and titles of IV + EU trials to then sort by country/sponsor/etc for title matching
# Generates pairings of EU and IV trials and their respective titles based on title matching algorithm

##########################################################

library(dplyr)
library(tidyr)
library(readr)
library(here)
library(fs)
library(lubridate)
library(stringr)
library(stringdist)
library(ctregistries)
library(cli)

# Get trials, limited to those meeting intovalue criteria
# `title` field in `trials` is full title for DRKS, but brief_title for ClinicalTrials.gov
# See IntoValue codebook: https://github.com/maia-sh/intovalue-data/blob/main/data/processed/codebook.csv
source(here::here("scripts", "functions", "filter-intovalue-criteria.R"))
trials <- read_iv_trials()

# Get official_title for ClinicalTrials.gov for title matching
# See related work: https://doi.org/10.1371/journal.pone.0193088
studies <-
  read_csv(here("data", "raw", "registries", "ctgov", "studies.csv")) |>

  # Limit to trials meeting IntoValue exclusion criteria
  semi_join(trials, by = c("nct_id" = "id"))

# Download EU trial protocol data dump
# Note: Manually save in corresponding filepath
eu_protocol_dump <-
  read_csv(here("data", "raw", "registries", "euctr", "euctr_euctr_dump-2024-02-03-054239.csv"))


##########################################################

# Extract IDs and process titles for IV trials first
iv_ids <-
  trials |>
  select(id, title) |>

  # Update NCT titles in iv_ids
  # Note: DRKS titles in iv_ids are NOT brief. This column name refers to the brief titles of ClinicalTrials.gov trials. The DRKS titles will not be replaced when the brief titles of NCT trials are replaced with their full titles
  rename(brief = title)

nct_full_titles <- studies |>
  select(nct_id, official_title) |>
  rename(id = nct_id) |>
  rename(official = official_title)

# If there is no official title available in ClinicalTrials.gov, the brief title will be used as the 'title'
iv_updated_titles <- left_join(iv_ids, nct_full_titles, by = "id") |>
  mutate(title = coalesce(official, brief)) |>
  select(-brief, -official)

# Process IV titles to make compatible with title matching algorithm
iv_updated_titles <- iv_updated_titles |>
  mutate(title_processed = tolower(title) |>
           stringr::str_squish() |> # remove whitespace at start and end, as well as any "\t" whitespace characters
           stringr::str_remove_all("[:punct:]") |>
           stringr::str_remove_all(" ")) |>
  select(id, title, title_processed)

###########################################################

# Extract EU IDs and country data (for filtering), plus process EU titles to be used in title matching algorithm
eu_ids <- eu_protocol_dump |>
          select(eudract_number,
                 member_state_concerned,
                 full_title_of_the_trial,
                 sponsors)

eu_ids <- rename(eu_ids, id = eudract_number, state = member_state_concerned, title = full_title_of_the_trial)

# Filter out EU trials which dont have 'Germany' listed in member_state_concerned
# Yields table with 13068 unique trials which mention Germany in this field
# Can filter however we want here.

eu_only_german <- eu_ids |>
                  filter(grepl('Germany', state))

# Process EU titles
eu_only_german <- eu_only_german |> select(id,title) |>
  tidyr::drop_na(title) |>
  distinct(id, title,.keep_all = TRUE) |>
  mutate(title_processed = tolower(title) |>
           stringr::str_squish() |> # remove whitespace at start and end, as well as any "\t" whitespace characters
           stringr::str_remove_all("[:punct:]") |>
           stringr::str_remove_all(" "))

##########################################################
## Title matching (from 02_cross-reg-title-matching.R)

# assigning maxDist value for title matching (Elements in x will not be matched
# with elements of table if their distance is larger than maxDist)
DISTANCE <- 10

## The below takes about 3.5 hours on my baseline 2020 MacBook Air
## Let it run, it takes a while!!

start_time <- Sys.time()

# use amatch function to find title matches
# Default method used for matching. Documentation for amatch function:
# https://www.rdocumentation.org/packages/stringdist/versions/0.9.12/topics/amatch
 title_matches <- cbind(
  euctr_title_tm = eu_only_german$title,
  euctr_id = eu_only_german$id,
  iv_updated_titles[amatch(
    eu_only_german$title_processed,
    iv_updated_titles$title_processed,
    maxDist = DISTANCE, matchNA = FALSE),]
) |>
  # keep only those where a match was found
  filter(
    !is.na(id)
  ) |>
  distinct(
    euctr_id, # preserve all unique combinations of EUCTR number and ID
    id,
    .keep_all = TRUE
  ) |>
  # add flag for EUCTR crossreg found via title matching
  mutate(
    has_crossreg_eudract_tm = TRUE
  )

end_time <- Sys.time()
total_time <- end_time - start_time


# store title matching results in distance specific dataframe
assign(paste0("title_matches_", DISTANCE), title_matches)

##########################################################

# Save as rds (will overwrite previous version)
dir_crossreg <- fs::dir_create(here::here("data", "cross-registrations"))
readr::write_rds(title_matches_10, fs::path(dir_crossreg, "title-matched-10.rds"))



