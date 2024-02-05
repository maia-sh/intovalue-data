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

#EU_ids = EU_dump %>% select(eudract_number) # %>% unique() # a lot of these are repeats because they only differ in country code
#EU_ids = rename(EU_ids, id = eudract_number)

# combine into larger, union() gets rid of duplicates
#combined_ids = union(IV_ids, EU_ids) #%>% unique()

##########################################################

# Clean and prepare IV trns separately before merging with EU

IV_clean = cross_registrations %>%
  filter(is_crossreg_reg) %>%
  group_by(id) %>%
  summarize(trns_reg = paste(crossreg_trn, collapse = ";")) %>%
  mutate(sponsor_s_protocol_code_number = NA) %>%
  relocate(sponsor_s_protocol_code_number, .before = trns_reg) %>%
  mutate(unclean_ids = NA) %>%
  mutate(is_IV = TRUE)


##########################################################

# Clean and prepare EU trns separately before merging with IV_clean

EU_clean = EU_dump %>% select(eudract_number, sponsor_s_protocol_code_number,
                                  isrctn_international_standard_randomised_controlled_trial_numbe,us_nct_clinicaltrials_gov_registry_number,
                                  who_universal_trial_reference_number_utrn, other_identifiers)

# Columns we want cleaned by Maia's script
columns_to_clean = c("isrctn_international_standard_randomised_controlled_trial_numbe",
                     "us_nct_clinicaltrials_gov_registry_number", "who_universal_trial_reference_number_utrn",
                     "other_identifiers")

# Column which should store the identifiers which cannot be cleaned by Maia's script
EU_clean$unclean_ids <- NA

# Clean table column by column
for (col in columns_to_clean) {
  cleaned_trns <- vector("list", length = nrow(EU_clean))
  unclean_ids <- vector("list", length = nrow(EU_clean))

  # 'trn' is the id in each row currently being cleaned
  for (i in seq_len(nrow(EU_clean))) {

    trn <- EU_clean[i, col]

    # Detects whether cleaning the string 'trn' throws an error. If it does, it replaces the TRN with NA
    cleaned <- tryCatch(clean_trn(trn, quiet = TRUE), error = function(e) trn)

    # Cleaner does NOT always throw an error with garbage IDs and instead just removes spaces, messing up our logic
    # Catch those mistakes here:
    trn_comparison <- gsub("\\s", "", tolower(trn))
    cleaned_comparison <- gsub("\\s", "", tolower(cleaned))

    if(trn_comparison == cleaned_comparison || is.na(trn_comparison)) {
      cleaned_trns[[i]] <- NA
      unclean_ids[[i]] <- trn
    }
    else {
      # Cleaned will receive either the cleaned version of a TRN or an NA. If the TRN was uncleanable, it goes to unclean_ids
      # if the cleaned TRN and the original TRN are the same, unclean_ids is blank (NA) for that row
      cleaned_trns[[i]] <- ifelse(identical(tolower(cleaned), tolower(trn)), NA, cleaned)
      unclean_ids[[i]] <- ifelse(identical(tolower(cleaned), tolower(trn)), NA, trn)
    }

  }
  # Cleaned TRNs are placed back in the column they belong, unclean ids sent to unclean_ids column
  EU_clean[, col] <- unlist(cleaned_trns)
  EU_clean$unclean_ids <- unlist(unclean_ids)

}

##########################################################
## Clean the last few stragglers from unclean_ids not caught by algorithm and put in trns_reg

# 2011-000911-26 has 'DKRS00000766' (typo) in other_identifiers, won't be caught by algorithm
# 2010-019181-91 has valid NCT and DRKS number, prob easier just to enter manually
# also 2013-000577-77 ; 2020-005450-18

# DRKS stragglers have format 'Name: DRKS Number: 00003246'
# NCT stragglers have format 'Name: NCT Number: 03351608'

# Function to identify and clean DRKS stragglers
clean_drks_number <- function(string) {
  drks_match <- regmatches(string, regexpr("DRKS Number: (\\d+)", string))

  # If a match is found, return the cleaned value, otherwise, return NA
  if (length(drks_match) < 1) {
    return(NA)
  }
  else if (length(drks_match[[1]]) > 0) {
    cleaned_value <- paste0("DRKS", gsub("\\D", "", drks_match[[1]]))
    return(cleaned_value)
  } else {
    return(NA)
  }
}

# Function to identify and clean NCT stragglers
clean_nct_number <- function(string) {

  # Use regex to extract the NCT number
  nct_match <- regmatches(string, regexpr("NCT Number: (\\d+)", string))

  # If a match is found, return the cleaned value, otherwise, return NA
  if (length(nct_match) < 1) {
    return(NA)
  }
  else if (length(nct_match[[1]]) > 0) {
    cleaned_value <- paste0("NCT", gsub("\\D", "", nct_match[[1]]))
    return(cleaned_value)
  } else {
    return(NA)
  }
}

# Apply the cleaning functions to the unclean_ids column
EU_clean$other_identifiers_drks <- sapply(EU_clean$unclean_ids, clean_drks_number)
EU_clean$other_identifiers_nct <- sapply(EU_clean$unclean_ids, clean_nct_number)

# Add new cleaned values to the existing values in a semicolon-separated list
EU_clean$other_identifiers <- ifelse(
  !is.na(EU_clean$other_identifiers_nct) | !is.na(EU_clean$other_identifiers_drks),
  paste(EU_clean$other_identifiers_nct, EU_clean$other_identifiers_drks, sep = "; "),
  EU_clean$other_identifiers
)

# Remove unnecessary columns
EU_clean <- EU_clean[, !(names(EU_clean) %in% c("other_identifiers_nct", "other_identifiers_drks"))]

##########################################################

# merge all connected TRNs into "trns_reg", like in IV_clean, separate protocol number, and add is_IV boolean to see if
# any of the EU trials are also in IV (none are)

EU_clean = unite(EU_clean, "trns_reg",isrctn_international_standard_randomised_controlled_trial_numbe,
                 us_nct_clinicaltrials_gov_registry_number, who_universal_trial_reference_number_utrn,
                 other_identifiers, sep = ";", na.rm = TRUE) %>%
  rename(id = eudract_number) %>%
  relocate(sponsor_s_protocol_code_number, .before = trns_reg) %>%
  mutate(is_IV = id %in% IV_ids$id)


##########################################################

# now append EU_clean to IV_clean to create full TRN_registry_data table and save
TRN_duplicates = rbind(EU_clean, IV_clean)

# Removes only rows that are exact duplicates of each other. No loss of data from differing sponsor protocol numbers
TRN_registry_data = TRN_duplicates[!duplicated(TRN_duplicates),]

saveRDS(TRN_registry_data, "TRN(registry data).rds" )
