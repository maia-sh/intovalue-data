## Script to combine all TRNs of IntoValue and EU trials.
## we are building our TRN (registry data) table, which will be a list of these TRNs along with the other TRNs that are mentioned in their
## registries, and also the sponsor protocol number associated with each IV or EU TRN, if available. We will also record which TRNs are in IV
## or not, so we can easily prioritize potential cross-registrations later.

##########################################################

library(dplyr)
library(tidyr)
library(readr)
library(here)
library(fs)
library(lubridate)
library(stringr)
library(ctregistries)

dir_raw <- here("data", "raw")
dir_processed <- here("data", "processed")

# full set of IV trials downloaded here so we can easily check which TRNs are in IV
trials <- read_csv(path(dir_processed, "trials.csv"))
cross_registrations <- read_rds(path(dir_processed, "trn", "cross-registrations.rds"))

## download EU trial data dump
EU_dump <- read_csv(path(dir_raw, "euctr_euctr_dump-2023-12-02-002347.csv"))

##########################################################

# extract only IDs from each of the big tables
IV_ids = trials %>% select(id) %>% unique()

#CR_ids = cross_registrations %>% select(crossreg_trn) %>% unique()
#CR_ids = rename(CR_ids, id = crossreg_trn)

EU_ids = EU_dump %>% select(eudract_number) # %>% unique() # a lot of these are repeats because they only differ in country code
EU_ids = rename(EU_ids, id = eudract_number)

# combine into larger, union() gets rid of duplicates
#combined_ids = union(IV_ids, EU_ids) #%>% unique()

##########################################################

# Clean and prepare IV trns separately before merging with EU

IV_clean = cross_registrations %>%
  filter(is_crossreg_reg) %>%
  group_by(id) %>%
  summarize(trns_reg = paste(crossreg_trn, collapse = ";")) %>%
  mutate(protocol_number = NA) %>%
  relocate(protocol_number, .before = trns_reg) %>%
  mutate(is_IV = TRUE)

##########################################################

# Clean and prepare EU trns separately before merging with IV_clean

# Maybe an unnecessary variable for now, but we want to collect sponsor/ country info for prioritization in title matching
EU_trns_reg = EU_dump %>% select(eudract_number,sponsor_s_protocol_code_number,
                                 isrctn_international_standard_randomised_controlled_trial_numbe,
                                 us_nct_clinicaltrials_gov_registry_number, who_universal_trial_reference_number_utrn,
                                 other_identifiers, member_state_concerned)

EU_clean = EU_trns_reg %>% select(eudract_number, sponsor_s_protocol_code_number,
                                  isrctn_international_standard_randomised_controlled_trial_numbe, us_nct_clinicaltrials_gov_registry_number,
                                  who_universal_trial_reference_number_utrn, other_identifiers)

columns_to_clean = c("isrctn_international_standard_randomised_controlled_trial_numbe",
                     "us_nct_clinicaltrials_gov_registry_number", "who_universal_trial_reference_number_utrn",
                     "other_identifiers")

# apply Maia's cleaning function to each column we want cleaned in EU table. All inputs that are uncleanable are returned the same
EU_clean[columns_to_clean] = apply(EU_clean[columns_to_clean], c(1, 2), function(x) tryCatch(clean_trn(x, quiet = TRUE), error = function(e) x))

# merge all connected TRNs into "trns_reg", like in IV_clean, separate protocol number, and add is_IV boolean to see if
# any of the EU trials are also in IV (none are)

EU_clean = unite(EU_clean, "trns_reg",isrctn_international_standard_randomised_controlled_trial_numbe,
                 us_nct_clinicaltrials_gov_registry_number, who_universal_trial_reference_number_utrn,
                 other_identifiers, sep = ";", na.rm = TRUE) %>%
  rename(protocol_number = sponsor_s_protocol_code_number) %>%
  rename(id = eudract_number) %>%
  relocate(protocol_number, .before = trns_reg) %>%
  mutate(is_IV = id %in% IV_ids$id)

##########################################################

# now append EU_clean to IV_clean to create full TRN_registry_data table and save
TRN_registry_data = rbind(EU_clean, IV_clean)

saveRDS(TRN_registry_data, "TRN(registry data).rds" )
