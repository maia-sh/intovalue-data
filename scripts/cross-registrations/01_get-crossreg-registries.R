# Find potential cross-registrations from all identifiers in IntoValue and EUCTR.
# Identifiers include TRNs as well as others, e.g., sponsor protocol number.
# Captures potential matches:
# * IV TRN in EUCTR
# * EUCTR TRN in IV registry (DRKS/ClinicalTrials.gov)
# * other id in EUCTR protocol or result in IV registry (DRKS/ClinicalTrials.gov)

library(dplyr)
library(tidyr)
library(readr)
library(here)
library(fs)
library(lubridate)
library(stringr)
library(ctregistries)
library(cli)


# Get data ----------------------------------------------------------------

dir_raw <- here("data", "raw")

# Get trials and cross-registrations, limited to those meeting intovalue criteria
source(here::here("scripts", "functions", "filter-intovalue-criteria.r"))
trials <- read_iv_trials()
cross_registrations <- read_iv_cross_registrations()

# Get all ids associated with intovalue trials from drks and ctgov.
# IDs include any identifiers or numbers assigned to a clinical study by a study's sponsor, funder, or others.
# May include unique identifiers from other trial registries and National Institutes of Health grant numbers.
# Study sponsor ID numbers are most likely to get additional matches when joining to sponsor_s_protocol_code_number.

ids_ctgov <-
  read_rds(here("data", "processed", "registries", "ctgov", "ctgov-ids.rds")) |>
  rename(other_id = id_value) |>

  # Some duplicated other_id missing id_type in single occurence
  group_by(nct_id, other_id) |>
  fill(id_type, .direction = "updown") |>
  ungroup()

ids_drks <-
  read_rds(here("data", "processed", "registries", "drks", "drks-ids.rds")) |>
  rename(other_id = id) |>

  # Recode missing values to NA and drop
  mutate(other_id = if_else(other_id %in% c("[---]* ", "/ "), NA_character_, other_id)) |>
  drop_na(other_id)

ids_linked <-
  bind_rows(
    rename(ids_ctgov, id = nct_id),
    rename(ids_drks, id = drks_id)
  ) |>
  rename(other_id_type = id_type) |>
  select(-raw_trn) |>

  # Remove exact duplicates (not sure why these appear)
  distinct() |>

  # Limit to cross-registrations of trials meeting IntoValue exclusion criteria
  semi_join(trials, by = "id") |>

  # Combine trn and other_id, prefering trn
  mutate(
    other_id = coalesce(trn, other_id),
    other_id_type = coalesce(registry, other_id_type)
  ) |>
  select(id, other_id, other_id_type)

# Get EU trial protocol data dump
# Note: Manually save in corresponding filepath
eu_protocol_dump <-
  read_csv(path(dir_raw, "registries", "euctr", "euctr_euctr_dump-2024-02-03-054239.csv"))

# Get EU trial results data dump
# Note: Manually save in corresponding filepath
eu_results_dump <-
  read_csv(path(dir_raw, "registries", "euctr", "euctr_data_quality_results_scrape_feb_2024.csv")) |>

  # Limit to trials in eu_protocol
  # Note: There is at least one trial that is in eu_results that is NOT in eu_protocol: 2006-005253-30
  semi_join(eu_protocol_dump, by = c("trial_id" = "eudract_number"))


##########################################################

# Clean and prepare IV trns separately before merging with EU

iv_clean <-
  cross_registrations |>
  filter(is_crossreg_reg) |>
  distinct(id, crossreg_trn, .keep_all = TRUE) |>
  group_by(id) |>
  summarize(trns_reg = paste(crossreg_trn, collapse = ";")) |>
  mutate(protocol_sponsor_code = NA) |>
  mutate(results_sponsor_code = NA) |>
  relocate(protocol_sponsor_code, .before = trns_reg) |>
  relocate(results_sponsor_code, .before = trns_reg) |>
  mutate(is_primary_IV_id = TRUE)

##########################################################

# Clean and prepare EU protocol trns separately before merging with iv_clean

eu_protocol_clean <-
  eu_protocol_dump |>
  select(eudract_number,
         protocol_sponsor_code = sponsor_s_protocol_code_number,
         isrctn_number = isrctn_international_standard_randomised_controlled_trial_numbe,
         nct_number = us_nct_clinicaltrials_gov_registry_number,
         who_utn_number = who_universal_trial_reference_number_utrn,
         other_ids = other_identifiers) |>
  mutate(
    isrctn_number_protocol_unclean = NA_character_,
    nct_number_protocol_unclean = NA_character_,
    other_ids_protocol_unclean = NA_character_
  )

# Columns we want cleaned by Maia's script
# WHO UTN is not cleaned, since it is not a registration number:
# The aim of the Universal Trial Number (UTN) is to facilitate the unambiguous identification of clinical trials.
# The UTN is not a registration number. (see https://www.who.int/clinical-trials-registry-platform/unambiguous-trial-identification/the-universal-trial-number-(utn))

columns_to_clean <- c("isrctn_number", "nct_number", "other_ids")

# Add progress bar (Be patient, progress bar takes at least 10 minutes to display at all)

cli_progress_bar(name = "Cleaning EU protocol data columns (3 total) : ", total = 3)

# Clean table column by column
for (col in columns_to_clean) {


  cleaned_trns <- vector("list", length <- nrow(eu_protocol_clean))

  # 'trn' is the id in each row currently being cleaned
  for (i in 1:nrow(eu_protocol_clean)) {

    trn <- eu_protocol_clean[[i, col]]

    # Detects whether cleaning the string 'trn' throws an error. If yes, initialize 'cleaned' with "Error"
    # The clean_trn() function is from the ctregistries package (https://github.com/maia-sh/ctregistries)
    # clean_trn() takes a single messy TRN as input. Currently, it is able to clean TRNs from ANZCTR, CT.gov, DRKS, ISRCTN, JapicCTI, EudraCT, NTR, and PACTR
    # It returns either single clean TRN, an error if the TRN is not recognised as associated with either of the above registries, or NA if the TRN is NA.

    if(is.na(trn)) {
      cleaned_trns[[i]] <- NA
      unclean_col <- paste0(col, "_protocol_unclean")
      eu_protocol_clean[[unclean_col]][i] <- trn
      next
    }

     cleaned <- tryCatch(clean_trn(trn, quiet = TRUE), error = function(e) "Error")

     # If the TRN can't be cleaned, eliminate it from cleaned column and place in corresponding unclean column for later evaluation
    if(cleaned == "Error") {
      cleaned_trns[[i]] <- NA
      unclean_col <- paste0(col, "_protocol_unclean")
      eu_protocol_clean[[unclean_col]][i] <- trn # Would be a garbage number
    }

    # If 'trn' can be cleaned, save it and discard the old TRN
    else {
      cleaned_trns[[i]] <- cleaned
      unclean_col <- paste0(col, "_protocol_unclean")
      eu_protocol_clean[[unclean_col]][i] <- NA
    }
  }

  # Sanity check that cleaned_trns vector has the same length as number of rows in column we want to merge it with
  rows_eu_clean = nrow(eu_protocol_clean[, col])
  length_cleaned = length(cleaned_trns)

  if(rows_eu_clean == length_cleaned) {
    # Cleaned TRNs are placed back in the column they belong
    eu_protocol_clean[, col] <- unlist(cleaned_trns)

    # Update progress bar
    cli_progress_update()
  }
  else {
    print("The cleaned vector is not of the same length as the row we want to combine it with")
  }
}

# Terminate progress bar
cli_progress_done()


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
  else if (length(drks_match) > 0) {
    cleaned_value <- paste0("DRKS", gsub("\\D", "", drks_match))
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
  else if (length(nct_match) > 0) {
    cleaned_value <- paste0("NCT", gsub("\\D", "", nct_match))
    return(cleaned_value)
  } else {
    return(NA)
  }
}

# Function to identify and clean ISRCTN stragglers in the isrctn_number_protocol_unclean column
clean_isrctn_number <- function(string) {

  if (grepl("^\\d{8}$", string)) {
    return(paste0("ISRCTN", string)) # return ISRCTN with prefix attached if pattern of 8 integers with no spaces matches
  }
  else {
    return(NA) # return NA if no match is found (returning NA b/c we're storing the results of this function in a new column, not just overwriting unclean data with an NA)
  }
}

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
    cleaned_value <- paste(unlist(matches), collapse = ";")
    return(cleaned_value)
  } else {
    return(string)  # Return the original value if no matches are found
  }
}

# First, run Dutch cleaning function on other_ids column, as these slip through to the supposedly clean column while
# actually still being dirty
eu_protocol_clean$other_ids <- sapply(eu_protocol_clean$other_ids, clean_nederlands_number)

# Apply the cleaning functions to the unclean 'other_ids' column, as
# all the NCT and DRKS numbers from the other_ids column that were uncleanable were shunted here after the first cleaning step.

eu_protocol_clean$other_identifiers_drks <- sapply(eu_protocol_clean$other_ids_protocol_unclean, clean_drks_number)
eu_protocol_clean$other_identifiers_nct <- sapply(eu_protocol_clean$other_ids_protocol_unclean, clean_nct_number)

# Add new cleaned values to the existing values in other_ids in a semicolon-separated list
eu_protocol_clean <- eu_protocol_clean |>
  mutate(
    other_ids = case_when(

      # Case where other_ids is NA, don't paste other_ids in, just paste  cleaned values to avoid introducing NAs
      !is.na(other_identifiers_nct) & !is.na(other_identifiers_drks) & is.na(other_ids) ~ paste(other_identifiers_nct, other_identifiers_drks, sep = ";"),
      !is.na(other_identifiers_nct) & is.na(other_ids) ~ other_identifiers_nct,
      !is.na(other_identifiers_drks) & is.na(other_ids) ~ other_identifiers_drks,

      !is.na(other_identifiers_nct) & !is.na(other_identifiers_drks) ~ paste(other_ids, other_identifiers_nct, other_identifiers_drks, sep = ";"),
      !is.na(other_identifiers_nct) ~ paste(other_ids, other_identifiers_nct, sep = ";"),
      !is.na(other_identifiers_drks) ~ paste(other_ids, other_identifiers_drks, sep = ";"),

      TRUE ~ other_ids
    )
  ) |>
  mutate(
    other_ids = na_if(other_ids, "NA")
  )

# Finally, apply ISRCTN cleaning function to isrctn_number_protocol_unclean column, store results in new column (isrctn_stragglers), then add to column isrctn_number if isrctn_stragglers is not NA
eu_protocol_clean$isrctn_stragglers <- sapply(eu_protocol_clean$isrctn_number_protocol_unclean, clean_isrctn_number)

# Adding new values to isrctn_number column
eu_protocol_clean <- eu_protocol_clean |>
  mutate(
    isrctn_number = case_when(
      !is.na(isrctn_stragglers) & is.na(isrctn_number) ~ isrctn_stragglers,
      !is.na(isrctn_stragglers) ~ paste(isrctn_number, isrctn_stragglers, sep = ";"),
      TRUE ~ isrctn_number
    )
  ) |>
  mutate(
    isrctn_number = na_if(isrctn_number, "NA")
  )

# Remove unnecessary columns
eu_protocol_clean <-
  eu_protocol_clean |>
  select(-c("other_identifiers_nct", "other_identifiers_drks", "isrctn_stragglers"))


##########################################################

# Clean EU results data and merge whatever extra information we get from this into our bigger EU table before merging that with our IV table
eu_results_clean <- eu_results_dump |>
  select(trial_id,
         nct_number,
         isrctn_number,
         who_utn_number,
         spon_prot_number,
         other_ids) |>
  mutate(
    isrctn_number_protocol_unclean = NA_character_,
    nct_number_protocol_unclean = NA_character_,
    other_ids_protocol_unclean = NA_character_
  )

# There are some stragglers from running cleaning function:
# like 2020-002109-24, where a valid Chinese and a valid Japanese ID are just smushed together. Must figure out how to deal with this.
# multiple valid IDs in a list sometimes don't get recognized: 2021-000904-39

# Add progress bar (Be patient, progress bar takes at least 10 minutes to display at all)

cli_progress_bar(name = "Cleaning EU results data columns (3 total) : ", total = 3)

# Clean table column by column
for (col in columns_to_clean) {


  cleaned_trns <- vector("list", length <- nrow(eu_results_clean))

  # 'trn' is the id in each row currently being cleaned
  for (i in 1:nrow(eu_results_clean)) {

    trn <- eu_results_clean[[i, col]]

    # Detects whether cleaning the string 'trn' throws an error. If yes, initialize 'cleaned' with "Error"
    # The clean_trn() function is from the ctregistries package (https://github.com/maia-sh/ctregistries)
    # clean_trn() takes a single messy TRN as input. Currently, it is able to clean TRNs from ANZCTR, CT.gov, DRKS, ISRCTN, JapicCTI, EudraCT, NTR, and PACTR
    # It returns either single clean TRN, an error if the TRN is not recognised as associated with either of the above registries, or NA if the TRN is NA.

    if(is.na(trn)) {
      cleaned_trns[[i]] <- NA
      unclean_col <- paste0(col, "_results_unclean")
      eu_results_clean[[unclean_col]][i] <- trn
      next
    }

    cleaned <- tryCatch(clean_trn(trn, quiet = TRUE), error = function(e) "Error")

    # If the TRN can't be cleaned, eliminate it from cleaned column and place in corresponding unclean column for later evaluation
    if(cleaned == "Error") {
      cleaned_trns[[i]] <- NA
      unclean_col <- paste0(col, "_results_unclean")
      eu_results_clean[[unclean_col]][i] <- trn # Would be a garbage number
    }

    # If 'trn' can be cleaned, save it and discard the old TRN
    else {
      cleaned_trns[[i]] <- cleaned
      unclean_col <- paste0(col, "_results_unclean")
      eu_results_clean[[unclean_col]][i] <- NA
    }
  }

  # Sanity check that cleaned_trns vector has the same length as number of rows in column we want to merge it with
  rows_eu_clean = nrow(eu_results_clean[, col])
  length_cleaned = length(cleaned_trns)

  if(rows_eu_clean == length_cleaned) {
    # Cleaned TRNs are placed back in the column they belong
    eu_results_clean[, col] <- unlist(cleaned_trns)

    # Update progress bar
    cli_progress_update()
  }
  else {
    print("The cleaned vector is not of the same length as the row we want to combine it with")
  }
}

# Terminate progress bar
cli_progress_done()



##########################################################
## Clean the Dutch stragglers from eu_results_clean not caught by algorithm and put in trns_reg
# Apply the cleaning functions to the other_ids column
eu_results_clean$other_ids <- sapply(eu_results_clean$other_ids, clean_nederlands_number)

##########################################################

# merge all connected TRNs into "trns_reg" in both protocol and results EU tables, like in iv_clean, separate protocol number, and add is_primary_IV_id boolean to record which TRNs are also in Into Value
eu_protocol_clean <- unite(eu_protocol_clean,
                          "trns_reg_protocol",
                          isrctn_number,
                          nct_number,
                          other_ids,
                          sep = ";",
                          na.rm = TRUE) |>
                    rename(id = eudract_number) |>
                    rename(who_utn_number_protocol = who_utn_number) |>
                    relocate(protocol_sponsor_code, .before = trns_reg_protocol) |>
                    mutate(is_primary_IV_id = id %in% trials$id)

eu_results_clean <- unite(eu_results_clean,
                         "trns_reg_results",
                         nct_number,
                         isrctn_number,
                         other_ids,
                         sep = ";",
                         na.rm = TRUE) |>
                    rename(id = trial_id) |>
                    rename(results_sponsor_code = spon_prot_number)

# Merge the 2 above tables into one master EU table. If trial IDs are repeated, concatenate the trns_reg field of each table into one new trns_reg column
# If there are duplicates in these lists, then add only 1 to the trns_reg column of master table (eu_clean)

eu_clean <- merge(eu_protocol_clean, eu_results_clean, by = "id", all.x = TRUE)

# Unite WHO UTN columns from the EU protocols and results tables

eu_clean <- unite(eu_clean,
                  who_utn_combined,
                  who_utn_number_protocol,
                  who_utn_number,
                  sep = ";",
                  na.rm = TRUE)

eu_clean$combined_trns_reg <- NA

# concatenate 'trns_reg' values from both tables together
for (i in 1:nrow(eu_clean)) {
  trns_reg1 <- eu_clean$trns_reg_protocol[i]
  trns_reg2 <- eu_clean$trns_reg_results[i]

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
  eu_clean$combined_trns_reg[i] <- combined_trns_reg
}

# remove duplicate values from the concatenated lists
eu_clean$combined_trns_reg <- sapply(eu_clean$combined_trns_reg, function(trns) {
  if (!is.na(trns)) {
    unique_trns <- unique(unlist(strsplit(trns, ";")))
    return(paste(sort(unique_trns), collapse = ";"))
  } else {
    return(NA)
  }
})

# dropping unnecessary columns
eu_clean <- subset(eu_clean, select = -c(trns_reg_protocol, trns_reg_results))

# separate out unclean columns and store them in the EU_unclean data frame
# keep combined WHO numbers here, since they are unnecessary for the TRN-TRN script and cause duplicate rows

EU_unclean <- eu_clean |>
  select(id,
         isrctn_number_protocol_unclean.x,
         isrctn_number_protocol_unclean.y,
         isrctn_number_results_unclean,
         nct_number_protocol_unclean.x,
         nct_number_protocol_unclean.y,
         nct_number_results_unclean,
         other_ids_protocol_unclean.x,
         other_ids_protocol_unclean.y,
         other_ids_results_unclean,
         who_utn_combined
  )

# keep only what you need in the EU clean data frame, and organise columns in order you want
eu_clean <- eu_clean |>
  select(id,
         protocol_sponsor_code,
         results_sponsor_code,
         combined_trns_reg,
         is_primary_IV_id,
  ) |>
  rename(
    trns_reg = combined_trns_reg
  )



##########################################################

# now append eu_protocol_clean to iv_clean to create full trn_registry_data table and save
trn_duplicates <- rbind(eu_clean, iv_clean)

# Removes only rows that are exact duplicates of each other. No loss of data from differing sponsor protocol numbers
trn_registry_data <- trn_duplicates[!duplicated(trn_duplicates),]

# Another reordering to make things neater before joining in ids.csv
trn_registry_data <- trn_registry_data |>
                    relocate(protocol_sponsor_code, .after = trns_reg) |>
                    relocate(results_sponsor_code, .after = protocol_sponsor_code)

##########################################################
# Now in one final addition of information, we will join in ids.csv
# See if id_value field in that table matches with our protocol_sponsor_code field or our results_sponsor_code field. If they match, bring in the value from the nct_id field in ids.csv table.

protocol_sponsor_linked_ids <- ids_linked |>
                              rename(protocol_sponsor_code = other_id) |>
                              rename(protocol_sponsor_linked_trn = id)

results_sponsor_linked_ids <- ids_linked |>
                             rename(results_sponsor_code = other_id) |>
                             rename(results_sponsor_linked_trn = id)

# Left join in sponsor linked TRNs from protocol data
trn_registry_data <- left_join(trn_registry_data, protocol_sponsor_linked_ids, by = "protocol_sponsor_code") |>
                    relocate(protocol_sponsor_linked_trn, .after = protocol_sponsor_code)

# Left join in sponsor linked TRNs from results data
trn_registry_data <- left_join(trn_registry_data, results_sponsor_linked_ids, by = "results_sponsor_code") |>
                    relocate(results_sponsor_linked_trn, .after = results_sponsor_code)

# Final handling of all empty cells or cells with value "NA" ; values set to logical NA
trn_registry_data <- trn_registry_data |>
                     mutate(trns_reg = na_if(trns_reg, ""))

# Save as rds (will overwrite previous version)
dir_crossreg <- fs::dir_create(here::here("data", "cross-registrations"))
readr::write_rds(trn_registry_data, fs::path(dir_crossreg, "trn-registry-data.rds"))
