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

dir_raw <- here("data", "raw")
dir_processed <- here("data", "processed")

# Download IV TRNs and titles. German full titles are here but the NCT titles here will need to be replaced by what's in studies.csv
trials <- read_csv(path(dir_processed, "trials.csv"))

# Download EU trial data dump, includes TRNs and full titles
EU_dump <- read_csv(path(dir_raw, "euctr_euctr_dump-2024-02-03-054239.csv"))

# Download studies.csv, which includes the 'official title' field for all NCTs
studies = read_csv(path(dir_raw,"studies.csv" ))

##########################################################

# extract IDs and Titles, and whatever information we want to filter by here for the EU trials.
# Since IV trials don't have country info like EU trials, we will just get the ID and title here
IV_ids = trials %>% select(id,title) %>% unique()

EU_ids = EU_dump %>% select(eudract_number, member_state_concerned, national_competent_authority, full_title_of_the_trial)
EU_ids = rename(EU_ids, id = eudract_number, state = member_state_concerned, title = full_title_of_the_trial, authority = national_competent_authority)

NCT_full_titles = studies %>%
                  select(nct_id, official_title) %>%
                  rename(id = nct_id) %>%
                  rename(title = official_title)

# Update NCT titles in IV_ids, leaving DRKS titles intact

IV_updated_titles <- left_join(IV_ids, NCT_full_titles, by = "id") %>%
                     mutate(title = ifelse(is.na(title.y), title.x, title.y)) %>%
                     select(-title.x, -title.y)

# Filter out EU trials which dont have 'Germany' listed in member_state_concerned AND national_competent authority
# Yields table with 13068 unique trials which mention Germany in both of these fields
# Can filter however we want here.

EU_only_German = EU_ids %>% filter(grepl('Germany', state) & grepl('Germany', authority))

##########################################################

# Process titles a bit before title matching

IV_updated_titles = IV_updated_titles %>% mutate(title_processed = tolower(title) |>
                               stringr::str_squish() |> # remove whitespace at start and end, as well as any "\t" whitespace characters
                               stringr::str_remove_all("[:punct:]") |>
                               stringr::str_remove_all(" ")) |>
                               select(id,
                               title,
                               title_processed)

EU_only_German = EU_only_German %>% select(id,title) |>
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
distance <- 7

# use amatch function to find title matches
title_matches <- cbind(
  euctr_title_tm = EU_only_German$title,
  euctr_id = EU_only_German$id,
  IV_updated_titles[amatch(
    EU_only_German$title_processed,
    IV_updated_titles$title_processed,
    maxDist = distance, matchNA = FALSE),]
) |>
  # keep only those where a match was found
  filter(
    !is.na(id)
  ) |>
  distinct(
    #id, euctr_id, # if we want to preserve all EUCTR crossreg for given trial
    id,
    .keep_all = TRUE
  ) |>
  # add flag for EUCTR crossreg found via title matching
  mutate(
    has_crossreg_eudract_tm = TRUE
  )

# store title matching results in distance specific dataframe
assign(paste0("title_matches_", distance), title_matches)

##########################################################

# Save to RDS to use in trn_trn script
saveRDS(title_matches_7, "title_matched_7.rds" )





