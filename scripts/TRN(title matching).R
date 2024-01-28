## Script to compile all TRNs and titles of IV + EU trials to then sort by country/sponsor/etc for title matching
## Uses similar structure to TRN(registry data), but includes additional information

##########################################################

library(dplyr)
library(tidyr)
library(readr)
library(here)
library(fs)
library(lubridate)
library(stringr)

dir_raw <- here("data", "raw")
dir_processed <- here("data", "processed")

# full set of IV trials downloaded here so we can easily check which TRNs are in IV
trials <- read_csv(path(dir_processed, "trials.csv"))
cross_registrations <- read_rds(path(dir_processed, "trn", "cross-registrations.rds"))

## download EU trial data dump
EU_dump <- read_csv(path(dir_raw, "euctr_euctr_dump-2023-12-02-002347.csv"))

##########################################################

# extract IDs and Titles, and whatever information we want to filter by here. We will just filter by member state here
# Since IV trials don't have country info like EU trials, we will just get the ID and title here
IV_ids = trials %>% select(id,title ) %>% unique() %>% mutate(is_IV = 1)


# Since we are filtering by country in this example and the IV trials have no country attached, we will filter
# all non-German trials out first and then rbind() with the IV table.
EU_ids = EU_dump %>% select(eudract_number, member_state_concerned, full_title_of_the_trial)
EU_ids = rename(EU_ids, id = eudract_number, state = member_state_concerned, title = full_title_of_the_trial) %>% mutate(is_IV = id %in% IV_ids$id)

# Filter step: Filter out all non-German TRNs (This is just an example, we can do anything we want here)
EU_German = EU_ids %>% filter(str_detect(state, "Germany")) %>% select(-state)

##########################################################

# combine EU_German and IV_ids to get the TRNs we will do title matching with
TRN_title_matching = rbind(EU_German, IV_ids)





