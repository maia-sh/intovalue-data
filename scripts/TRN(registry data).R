## Script to combine all TRNs of IntoValue and all (potential) Cross Registrations, and all EU trials.
## we are building our TRN (registry data) table, which will be a list of TRNs along with the other TRNs that are mentioned in their
## registries.

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
combined_ids = union(IV_ids, EU_ids) #%>% unique()

##########################################################

#now we need to add info about what other TRNs/other IDs are mentioned in the registry of each of these above trials

#for EU, we will collect all trial identifiers from EU_dump, including sponsor protocol number, NCT, and other identifiers if available

EU_trns_reg = EU_dump %>% select(eudract_number,sponsor_s_protocol_code_number,
                                 isrctn_international_standard_randomised_controlled_trial_numbe,
                                 us_nct_clinicaltrials_gov_registry_number, who_universal_trial_reference_number_utrn,
                                 other_identifiers
) # %>% unique()
EU_trns_reg = rename(EU_trns_reg, id = eudract_number)

TRN_registry_data = left_join(combined_ids, EU_trns_reg, by = "id")

# for NCT/ all other trials, we will fill out the table in the following way:
# Join TRN table and cross_registrations by ID, filter out all cross-registrations where the other TRN is not found in the IV registry
# TRNs may have multiple cross-reg associated with them, so group by each TRN and add all related TRNs to semicolon separated list in new
# column "all_crossregs"

cleaned_crossreg = left_join(TRN_registry_data,cross_registrations, by = "id") %>%
  filter(is_crossreg_reg) %>%
  group_by(id) %>%
  summarize(all_crossregs = paste(crossreg_trn, collapse = ";"))

# put all information from EU and IV crossregs together. Join first by ID
# create new "trns_reg" column which will contain all identifier information taken from both EU_dump and cross_registration united in single
# column "trns_reg", with each identifier separated by a semicolon.

TRN_registry_data = left_join(TRN_registry_data, cleaned_crossreg, by = "id")
TRN_registry_data = unite(TRN_registry_data, "trns_reg", all_crossregs,
                          isrctn_international_standard_randomised_controlled_trial_numbe,
                          us_nct_clinicaltrials_gov_registry_number, who_universal_trial_reference_number_utrn,
                          other_identifiers, sep = ";", na.rm = TRUE) %>% rename(protocol_number = sponsor_s_protocol_code_number)

##########################################################
# Now we will clean the trns in each row of this table, as some are coming to us with unnecessary characters or otherwise messy
# Maia's function will not be able to clean all of these TRNs, so some exception handling is necessary
# so some TRNs don't get replaced with error messages

TRN_registry_data$trns_reg <- sapply(TRN_registry_data$trns_reg, function(x) tryCatch(clean_trn(x, quiet = TRUE), error = function(e) x))

TRN_copy = TRN_registry_data

saveRDS(TRN_registry_data, "TRN(registry data).rds" )
