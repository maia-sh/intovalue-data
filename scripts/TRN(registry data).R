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

## download EU trial protocol data dump (get the newest version from Delwen's email)
EU_protocol_dump <- read_csv(path(dir_raw, "euctr_euctr_dump-2023-12-02-002347.csv")) # there is a new updated version from Nick, download this first

## download EU trial results data dump
EU_results_dump <- read_csv(path(dir_raw, "euctr_data_quality_results_scrape_feb_2024.csv" ))

##########################################################

# extract only IDs from each of the big tables
IV_ids = trials %>% select(id) %>% unique()

#CR_ids = cross_registrations %>% select(crossreg_trn) %>% unique()
#CR_ids = rename(CR_ids, id = crossreg_trn)

#EU_ids = EU_protocol_dump %>% select(eudract_number) # %>% unique() # a lot of these are repeats because they only differ in country code
#EU_ids = rename(EU_ids, id = eudract_number)

# combine into larger, union() gets rid of duplicates
# combined_ids = union(IV_ids, EU_ids) #%>% unique()

##########################################################

# Clean and prepare IV trns separately before merging with EU

IV_clean = cross_registrations %>%
  filter(is_crossreg_reg) %>%
  group_by(id) %>%
  summarize(trns_reg = paste(crossreg_trn, collapse = ";")) %>%
  mutate(sponsor_s_protocol_code_number = NA) %>%
  relocate(sponsor_s_protocol_code_number, .before = trns_reg) %>%
  mutate(unclean_ids = NA) %>%
  mutate(is_IV = TRUE) %>%
  mutate(who_universal_trial_reference_number_utrn = NA) %>%
  relocate(who_universal_trial_reference_number_utrn, .before = unclean_ids)


##########################################################

# Clean and prepare EU protocol trns separately before merging with IV_clean

EU_protocol_clean = EU_protocol_dump %>% select(eudract_number,
                                                sponsor_s_protocol_code_number,
                                                isrctn_international_standard_randomised_controlled_trial_numbe,
                                                us_nct_clinicaltrials_gov_registry_number,
                                                who_universal_trial_reference_number_utrn,
                                                other_identifiers)

# Columns we want cleaned by Maia's script
# got rid of WHO number column (who_universal_trial_reference_number_utrn) as Maia said to keep it separate, plus the well formatted numbers here still return NA with which_registry()
# which will mess up our logic

protocol_columns_to_clean = c("isrctn_international_standard_randomised_controlled_trial_numbe",
                              "us_nct_clinicaltrials_gov_registry_number",
                              "other_identifiers")

# Column which should store the identifiers which cannot be cleaned by Maia's script
EU_protocol_clean$unclean_ids <- NA

# Clean table column by column
for (col in protocol_columns_to_clean) {
  cleaned_trns <- vector("list", length = nrow(EU_protocol_clean))
  unclean_ids <- vector("list", length = nrow(EU_protocol_clean))

  # 'trn' is the id in each row currently being cleaned
  for (i in seq_len(nrow(EU_protocol_clean))) {

    trn <- EU_protocol_clean[i, col]

    # Detects whether cleaning the string 'trn' throws an error. If yes, initialize 'cleaned' with "Error"
    cleaned <- tryCatch(clean_trn(trn, quiet = TRUE), error = function(e) "Error")

    # Cleaner does NOT always throw an error with garbage IDs and instead just removes spaces, messing up our logic (duds)
    # Catch those mistakes here:

    trn_comparison <- gsub("\\s", "", tolower(trn))
    cleaned_comparison <- gsub("\\s", "", tolower(cleaned))

    # If the TRN can't be cleaned, eliminate it from cleaned column and place in unclean_ids for later evaluation
    if(cleaned == "Error" | is.na(trn)) {
      cleaned_trns[[i]] <- NA
      unclean_ids[[i]] <- trn # Could be either a garbage number or NA
    }
    # If the cleaned and original are the same AND the registry cannot be identified, it must be a dud (see above)
    else if ((trn_comparison == cleaned_comparison) & !(which_registry(cleaned) %in% c("ANZCTR", "ChiCTR", "ClinicalTrials.gov", "CRiS", "CTRI", "DRKS", "IRCT", "ISRCTN", "JapicCTI", "jRCT", "EudraCT", "NTR", "PACTR", "UMIN-CTR"))){
      cleaned_trns[[i]] <- NA
      unclean_ids[[i]] <- trn
    }
    # If the cleaned and original are the same and the registry CAN be identified, the original must have already been clean
    else if ((trn_comparison == cleaned_comparison) & (which_registry(cleaned) %in% c("ANZCTR", "ChiCTR", "ClinicalTrials.gov", "CRiS", "CTRI", "DRKS", "IRCT", "ISRCTN", "JapicCTI", "jRCT", "EudraCT", "NTR", "PACTR", "UMIN-CTR"))){
      cleaned_trns[[i]] <- cleaned
      unclean_ids[[i]] <- NA
    }
    # Cleaned and original must be different, so save cleaned and discard the old TRN
    else {
      cleaned_trns[[i]] <- cleaned
      unclean_ids[[i]] <- NA
    }

  }
  # Cleaned TRNs are placed back in the column they belong, unclean ids sent to unclean_ids column
  EU_protocol_clean[, col] <- unlist(cleaned_trns)
  EU_protocol_clean$unclean_ids <- unlist(unclean_ids)

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
EU_protocol_clean$other_identifiers_drks <- sapply(EU_protocol_clean$unclean_ids, clean_drks_number)
EU_protocol_clean$other_identifiers_nct <- sapply(EU_protocol_clean$unclean_ids, clean_nct_number)

# Add new cleaned values to the existing values in a semicolon-separated list
EU_protocol_clean$other_identifiers <- ifelse(
  !is.na(EU_protocol_clean$other_identifiers_nct) | !is.na(EU_protocol_clean$other_identifiers_drks),
  paste(EU_protocol_clean$other_identifiers_nct, EU_protocol_clean$other_identifiers_drks, sep = "; "),
  EU_protocol_clean$other_identifiers
)

# Remove unnecessary columns
EU_protocol_clean <- EU_protocol_clean[, !(names(EU_protocol_clean) %in% c("other_identifiers_nct", "other_identifiers_drks"))]


##########################################################

# Clean EU results data and merge whatever extra information we get from this into our bigger EU table before merging that with our
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

# Column which should store the identifiers which cannot be cleaned by Maia's script
EU_results_clean$unclean_ids <- NA

# Clean table column by column
for (col in results_columns_to_clean) {
  cleaned_trns <- vector("list", length = nrow(EU_results_clean))
  unclean_ids <- vector("list", length = nrow(EU_results_clean))

  # 'trn' is the id in each row currently being cleaned
  for (i in seq_len(nrow(EU_results_clean))) {

    trn <- EU_results_clean[i, col]

    # Detects whether cleaning the string 'trn' throws an error. If yes, initialize 'cleaned' with "Error"
    cleaned <- tryCatch(clean_trn(trn, quiet = TRUE), error = function(e) "Error")

    # Cleaner does NOT always throw an error with garbage IDs and instead just removes spaces, messing up our logic (duds)
    # Catch those mistakes here:

    trn_comparison <- gsub("\\s", "", tolower(trn))
    cleaned_comparison <- gsub("\\s", "", tolower(cleaned))

    # If the TRN can't be cleaned, eliminate it from cleaned column and place in unclean_ids for later evaluation
    if(cleaned == "Error" | is.na(trn)) {
      cleaned_trns[[i]] <- NA
      unclean_ids[[i]] <- trn # Could be either a garbage number or NA
    }
    # If the cleaned and original are the same AND the registry cannot be identified, it must be a dud (see above)
    else if ((trn_comparison == cleaned_comparison) & !(which_registry(cleaned) %in% c("ANZCTR", "ChiCTR", "ClinicalTrials.gov", "CRiS", "CTRI", "DRKS", "IRCT", "ISRCTN", "JapicCTI", "jRCT", "EudraCT", "NTR", "PACTR", "UMIN-CTR"))){
      cleaned_trns[[i]] <- NA
      unclean_ids[[i]] <- trn
    }
    # If the cleaned and original are the same and the registry CAN be identified, the original must have already been clean
    else if ((trn_comparison == cleaned_comparison) & (which_registry(cleaned) %in% c("ANZCTR", "ChiCTR", "ClinicalTrials.gov", "CRiS", "CTRI", "DRKS", "IRCT", "ISRCTN", "JapicCTI", "jRCT", "EudraCT", "NTR", "PACTR", "UMIN-CTR"))){
      cleaned_trns[[i]] <- cleaned
      unclean_ids[[i]] <- NA
    }
    # Cleaned and original must be different, so save cleaned and discard the old TRN
    else {
      cleaned_trns[[i]] <- cleaned
      unclean_ids[[i]] <- NA
    }


  }
  # Cleaned TRNs are placed back in the column they belong, unclean ids sent to unclean_ids column
  EU_results_clean[, col] <- unlist(cleaned_trns)
  EU_results_clean$unclean_ids <- unlist(unclean_ids)

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

# merge all connected TRNs into "trns_reg" in both protocol and results EU tables, like in IV_clean, separate protocol number, and add is_IV boolean to see if
# any of the EU trials are also in IV (none are)

EU_protocol_clean = unite(EU_protocol_clean,
                          "trns_reg",
                          isrctn_international_standard_randomised_controlled_trial_numbe,
                          us_nct_clinicaltrials_gov_registry_number,
                          other_identifiers,
                          sep = ";",
                          na.rm = TRUE) %>%
                    rename(id = eudract_number) %>%
                    rename(who_utn_number = who_universal_trial_reference_number_utrn) %>%
                    relocate(sponsor_s_protocol_code_number, .before = trns_reg) %>%
                    mutate(is_IV = id %in% IV_ids$id)

EU_results_clean = unite(EU_results_clean,
                         "trns_reg",
                         nct_number,
                         isrctn_number,
                         other_ids,
                         sep = ";",
                         na.rm = TRUE) %>%
                    rename(id = trial_id)

# Merge the 2 above tables into one master EU table. If trial IDs are repeated, concatenate the trns_reg field of each table into one new trns_reg column
# If there are duplicates in these lists, then add only 1 to the trns_reg column of master table (EU_clean)

EU_clean <- merge(EU_protocol_clean, EU_results_clean, by = "id", all.x = TRUE)

EU_clean$combined_trns_reg <- NA

# concatenate 'trns_reg' values from both tables
for (i in seq_len(nrow(EU_clean))) {
  trns_reg1 <- EU_clean$trns_reg.x[i]
  trns_reg2 <- EU_clean$trns_reg.y[i]

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

# dropping unnecessary columns, including WHO numbers for now, because of Maia's advice and for simplicity in table design
EU_clean <- subset(EU_clean, select = -c(trns_reg.x, trns_reg.y, unclean_ids.x, unclean_ids.y, who_utn_number.x, who_utn_number.y))
IV_clean <- subset(IV_clean, select = -c(who_universal_trial_reference_number_utrn, unclean_ids))

# renaming and reordering columns to make binding easier
EU_clean <- rename(EU_clean, trns_reg = combined_trns_reg) %>%
            relocate(is_IV, .after = trns_reg)
##########################################################

# now append EU_protocol_clean to IV_clean to create full TRN_registry_data table and save
TRN_duplicates = rbind(EU_clean, IV_clean)

# Removes only rows that are exact duplicates of each other. No loss of data from differing sponsor protocol numbers
TRN_registry_data = TRN_duplicates[!duplicated(TRN_duplicates),]

saveRDS(TRN_registry_data, "TRN(registry data).rds" )
