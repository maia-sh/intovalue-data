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

## download EU trial protocol data dump
EU_protocol_dump <- read_csv(path(dir_raw, "euctr_euctr_dump-2024-02-03-054239.csv"))

## download EU trial results data dump
# There is at least one trial that is in EU_results that is NOT in EU_protocol: 2006-005253-30
# In lieu of a better solution, I'll just delete this row from EU_results for now.

EU_results_dump <- read_csv(path(dir_raw, "euctr_data_quality_results_scrape_feb_2024.csv" ))
EU_results_dump <- EU_results_dump[EU_results_dump$trial_id != "2006-005253-30", ]

## Download ids table in ctgov folder from zenodo
# will be left joined by sponsor_s_protocol number to see if we get any more extra TRNs

sponsor_linked_ids = read_csv(path(dir_raw, "ids.csv"))
##########################################################

# extract only IDs from each of the big tables
IV_ids = trials %>% select(id) %>% unique()

##########################################################

# Clean and prepare IV trns separately before merging with EU

IV_clean = cross_registrations %>%
  filter(is_crossreg_reg) %>%
  group_by(id) %>%
  summarize(trns_reg = paste(crossreg_trn, collapse = ";")) %>%
  mutate(sponsor_s_protocol_code_number = NA) %>%
  relocate(sponsor_s_protocol_code_number, .before = trns_reg) %>%
  mutate(is_primary_IV_id = TRUE) %>%
  mutate(who_utn= NA)


##########################################################

# Clean and prepare EU protocol trns separately before merging with IV_clean

EU_protocol_clean = EU_protocol_dump %>% select(eudract_number,
                                                sponsor_s_protocol_code_number,
                                                isrctn_international_standard_randomised_controlled_trial_numbe,
                                                us_nct_clinicaltrials_gov_registry_number,
                                                who_universal_trial_reference_number_utrn,
                                                other_identifiers) %>%
                                          rename(isrctn_number =  isrctn_international_standard_randomised_controlled_trial_numbe,
                                                 nct_number = us_nct_clinicaltrials_gov_registry_number,
                                                 who_utn_number = who_universal_trial_reference_number_utrn,
                                                 other_ids = other_identifiers)

# Columns we want cleaned by Maia's script
# got rid of WHO number column (who_universal_trial_reference_number_utrn) as Maia said to keep it separate, plus the well formatted numbers here still return NA with which_registry()
# which will mess up our logic

protocol_columns_to_clean <- c("isrctn_number",
                               "nct_number",
                               "other_ids")

# Initialize separate unclean_ids columns for each column to be cleaned
for (col in protocol_columns_to_clean) {
  unclean_col <- paste0(col, "_protocol_unclean")
  EU_protocol_clean[[unclean_col]] <- NA_character_
}

# Clean table column by column
for (col in protocol_columns_to_clean) {
  cleaned_trns <- vector("list", length = nrow(EU_protocol_clean))

  # 'trn' is the id in each row currently being cleaned
  for (i in seq_len(nrow(EU_protocol_clean))) {

    trn <- EU_protocol_clean[i, col]

    # Detects whether cleaning the string 'trn' throws an error. If yes, initialize 'cleaned' with "Error"
    cleaned <- tryCatch(clean_trn(trn, quiet = TRUE), error = function(e) "Error")

    # Cleaner does NOT always throw an error with garbage IDs and instead just removes spaces, messing up our logic (duds)
    # Catch those mistakes here:

    trn_comparison <- gsub("\\s", "", tolower(trn))
    cleaned_comparison <- gsub("\\s", "", tolower(cleaned))

    # If the TRN can't be cleaned, eliminate it from cleaned column and place in corresponding unclean column for later evaluation
    if(cleaned == "Error" | is.na(trn)) {
      cleaned_trns[[i]] <- NA
      unclean_col <- paste0(col, "_protocol_unclean")
      EU_protocol_clean[[unclean_col]][i] <- trn # Could be either a garbage number or NA
    }
    # Cleaned and original must be different, so save cleaned and discard the old TRN
    else {
      cleaned_trns[[i]] <- cleaned
      unclean_col <- paste0(col, "_protocol_unclean")
      EU_protocol_clean[[unclean_col]][i] <- NA
    }
  }
  # Cleaned TRNs are placed back in the column they belong
  EU_protocol_clean[, col] <- unlist(cleaned_trns)
}


##########################################################
## Clean the last few stragglers not caught by algorithm and put in trns_reg

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
EU_protocol_clean$other_identifiers_drks <- sapply(EU_protocol_clean$other_ids_protocol_unclean, clean_drks_number)
EU_protocol_clean$other_identifiers_nct <- sapply(EU_protocol_clean$other_ids_protocol_unclean, clean_nct_number)

# Add new cleaned values to the existing values in other_ids in a semicolon-separated list
EU_protocol_clean$other_ids <- ifelse(
  !is.na(EU_protocol_clean$other_identifiers_nct) | !is.na(EU_protocol_clean$other_identifiers_drks),
  paste(EU_protocol_clean$other_identifiers_nct, EU_protocol_clean$other_identifiers_drks, sep = "; "),
  EU_protocol_clean$other_ids
)

# Remove unnecessary columns
EU_protocol_clean <- EU_protocol_clean[, !(names(EU_protocol_clean) %in% c("other_identifiers_nct", "other_identifiers_drks"))]


##########################################################

# Clean EU results data and merge whatever extra information we get from this into our bigger EU table before merging that with our IV table
EU_results_clean = EU_results_dump %>% select(trial_id,
                                              nct_number,
                                              isrctn_number,
                                              who_utn_number,
                                              other_ids)

# There are some stragglers from running cleaning function:
# like 2020-002109-24, where a valid Chinese and a valid Japanese ID are just smushed together. Must figure out how to deal with this.
# multiple valid IDs in a list sometimes don't get recognized: 2021-000904-39

# some stragglers with Dutch trial registries: 2012-001909-24
#  2017-002288-16, etc

results_columns_to_clean = c("nct_number",
                            "isrctn_number",
                            "other_ids")

# Initialize separate unclean_ids columns for each column to be cleaned
for (col in results_columns_to_clean) {
  unclean_col <- paste0(col, "_results_unclean")
  EU_results_clean[[unclean_col]] <- NA_character_
}

# Clean table column by column
for (col in results_columns_to_clean) {
  cleaned_trns <- vector("list", length = nrow(EU_results_clean))

  # 'trn' is the id in each row currently being cleaned
  for (i in seq_len(nrow(EU_results_clean))) {

    trn <- EU_results_clean[i, col]

    # Detects whether cleaning the string 'trn' throws an error. If yes, initialize 'cleaned' with "Error"
    cleaned <- tryCatch(clean_trn(trn, quiet = TRUE), error = function(e) "Error")

    # Cleaner does NOT always throw an error with garbage IDs and instead just removes spaces, messing up our logic (duds)
    # Catch those mistakes here:

    trn_comparison <- gsub("\\s", "", tolower(trn))
    cleaned_comparison <- gsub("\\s", "", tolower(cleaned))

    # If the TRN can't be cleaned, eliminate it from cleaned column and place in corresponding unclean column for later evaluation
    if(cleaned == "Error" | is.na(trn)) {
      cleaned_trns[[i]] <- NA
      unclean_col <- paste0(col, "_results_unclean")
      EU_results_clean[[unclean_col]][i] <- trn # Could be either a garbage number or NA
    }
    # Cleaned and original must be different, so save cleaned and discard the old TRN
    else {
      cleaned_trns[[i]] <- cleaned
      unclean_col <- paste0(col, "_results_unclean")
      EU_results_clean[[unclean_col]][i] <- NA
    }
  }
  # Cleaned TRNs are placed back in the column they belong
  EU_results_clean[, col] <- unlist(cleaned_trns)
}

##########################################################
## Clean the Dutch stragglers from EU_results_clean not caught by algorithm and put in trns_reg

# Dutch straggler formats:

# DutchTrialRegister:NL8152
# DutchTrialRegister(LTR):NL5458
# NederlandTrilaRegister:NTR4269
# NationalTrialregistration:NTR3912
# NederlandsTrialregister:NL3011
# NederlandsTrailRegister:NL4187/NTR4337
# EffectofDelmopinolontreatmentofinflammation:NL5159
# ECnumber:MEC-2013-310,NederlandTrilaRegister:NTR4269,CCMOdossiernumber:NL41789.078.13

# Function to identify and clean Dutch stragglers
clean_nederlands_number <- function(string) {

  matches <- regmatches(string, gregexpr("(NL\\d+|NTR\\d+)", string))

  if (length(matches) > 0 && length(unlist(matches)) > 0) {
    cleaned_value <- paste(unlist(matches), collapse = "; ")
    return(cleaned_value)
  } else {
    return(string)  # Return the original value if no matches are found
  }
}

# Apply the cleaning functions to the other_ids column
EU_results_clean$other_ids <- sapply(EU_results_clean$other_ids, clean_nederlands_number)

##########################################################

# merge all connected TRNs into "trns_reg" in both protocol and results EU tables, like in IV_clean, separate protocol number, and add is_primary_IV_id boolean to see if
# any of the EU trials are also in IV (none are)

EU_protocol_clean = unite(EU_protocol_clean,
                          "trns_reg_protocol",
                          isrctn_number,
                          nct_number,
                          other_ids,
                          sep = ";",
                          na.rm = TRUE) %>%
                    rename(id = eudract_number) %>%
                    rename(who_utn_number_protocol = who_utn_number) %>%
                    relocate(sponsor_s_protocol_code_number, .before = trns_reg_protocol) %>%
                    mutate(is_primary_IV_id = id %in% IV_ids$id)

EU_results_clean = unite(EU_results_clean,
                         "trns_reg_results",
                         nct_number,
                         isrctn_number,
                         other_ids,
                         sep = ";",
                         na.rm = TRUE) %>%
                    rename(id = trial_id)

# Merge the 2 above tables into one master EU table. If trial IDs are repeated, concatenate the trns_reg field of each table into one new trns_reg column
# If there are duplicates in these lists, then add only 1 to the trns_reg column of master table (EU_clean)

EU_clean <- merge(EU_protocol_clean, EU_results_clean, by = "id", all.x = TRUE)

# Unite WHO UTN columns from the EU protocols and results tables

EU_clean = unite(EU_clean, who_utn_combined, who_utn_number_protocol, who_utn_number, sep = ";")
EU_clean$combined_trns_reg <- NA

# concatenate 'trns_reg' values from both tables together
for (i in seq_len(nrow(EU_clean))) {
  trns_reg1 <- EU_clean$trns_reg_protocol[i]
  trns_reg2 <- EU_clean$trns_reg_results[i]

  # combine 'trns_reg' values, handling empty cells
  if (!is.na(trns_reg1) && trns_reg1 != "") {
    combined_trns_reg <- trns_reg1
  } else {
    combined_trns_reg <- ""
  }

  if (!is.na(trns_reg2) && trns_reg2 != "") {
    if (combined_trns_reg != "") {
      combined_trns_reg <- paste(combined_trns_reg, trns_reg2, sep = ";")
    } else {
      combined_trns_reg <- trns_reg2
    }
  }

  # store the concatenated value in the new column
  EU_clean$combined_trns_reg[i] <- combined_trns_reg
}

# remove duplicate values from the concatenated lists
EU_clean$combined_trns_reg <- sapply(EU_clean$combined_trns_reg, function(trns) {
  if (!is.na(trns)) {
    unique_trns <- unique(unlist(strsplit(trns, ";")))
    return(paste(unique_trns, collapse = ";"))
  } else {
    return(NA)
  }
})

# dropping unnecessary columns
EU_clean <- subset(EU_clean, select = -c(trns_reg_protocol, trns_reg_results))

# renaming and reordering columns to make binding easier
EU_clean <- rename(EU_clean, trns_reg = combined_trns_reg) %>%
            relocate(trns_reg, .after = sponsor_s_protocol_code_number) %>%
            relocate(is_primary_IV_id, .after = trns_reg)

# editing IV_clean to make binding possible
IV_clean = rename(IV_clean, who_utn_combined = who_utn) %>%
           mutate(isrctn_number_protocol_unclean = NA) %>%
           mutate(nct_number_protocol_unclean = NA) %>%
           mutate(other_ids_protocol_unclean = NA) %>%
           mutate(nct_number_results_unclean = NA) %>%
           mutate(isrctn_number_results_unclean = NA) %>%
           mutate(other_ids_results_unclean = NA)


##########################################################

# now append EU_protocol_clean to IV_clean to create full TRN_registry_data table and save
TRN_duplicates = rbind(EU_clean, IV_clean)

# Removes only rows that are exact duplicates of each other. No loss of data from differing sponsor protocol numbers
TRN_registry_data = TRN_duplicates[!duplicated(TRN_duplicates),]

##########################################################
# Now in one final addition of information, we will join in ids.csv
# See if id_value field in that table matches with our sponsor_s_protocol_code_number field. If they match, bring in the value from the nct_id field in ids.csv table

sponsor_linked_ids = sponsor_linked_ids %>%
                     rename(sponsor_s_protocol_code_number = id_value) %>%
                     rename(sponsor_linked_trn = nct_id) %>%
                     select(sponsor_linked_trn, sponsor_s_protocol_code_number)

TRN_registry_data = left_join(TRN_registry_data, sponsor_linked_ids, by = "sponsor_s_protocol_code_number") %>%
                    relocate(sponsor_linked_trn, .after = trns_reg)

## Save as RDS
saveRDS(TRN_registry_data, "TRN(registry data).rds" )
