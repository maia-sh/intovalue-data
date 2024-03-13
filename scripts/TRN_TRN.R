## Script to build TRN-TRN table for manual proofing using publications table and TRN(registry data) table

##########################################################

library(dplyr)
library(tidyr)
library(readr)
library(here)
library(fs)
library(lubridate)
library(stringr)
library(ctregistries)

# Load registry data
TRN_registry_data = read_rds("TRN(registry data).rds")

# Load title matching data
title_matches = read_rds("title_matched_7.rds")

# This is where we will load the publications table once its ready
publications = read_rds("publications_final.rds")

## Build our empty TRN-TRN table
trn_trn = data.frame(
  trn1 = numeric(),
  trn2 = numeric(),
  registry1 = character(),
  registry2 = character(),
  trn1inreg2 = logical(),
  trn2inreg1 = logical(),
  pub_si = logical(),
  pub_abs = logical(),
  pub_ft = logical(),
  is_match_protocol_sponsor_protocol_id = logical(),
  is_match_results_sponsor_protocol_id = logical(),
  is_title_matched = logical(),
  other = character(),
  crossreg_manual_valid = logical(),
  validation_date = as.Date(character(), format = "%Y-%m-%d"),
  comment = character(),
  stringsAsFactors = FALSE
)

####################################################################################################################
# Functions for adding and editing rows of trn_trn

# Only required parameters are trn1 and trn2. All else are NA by default, so that the structure of the table stays intact throughout adding/editing
add_trn_trn_row <- function(trn1, trn2, registry1 = NA, registry2 = NA,
                            trn1inreg2 = NA, trn2inreg1 = NA, pub_si = NA, pub_abs = NA,
                            pub_ft = NA, is_match_protocol_sponsor_protocol_id = NA, is_match_results_sponsor_protocol_id = NA, is_title_matched = NA,
                            other = NA, crossreg_manual_valid = NA, validation_date = NA, comment = NA) {

  # Create a new row with the provided parameters
  new_row <- data.frame(
    trn1 = trn1,
    trn2 = trn2,
    registry1 = as.character(registry1),
    registry2 = as.character(registry2),
    trn1inreg2 = as.logical(trn1inreg2),
    trn2inreg1 = as.logical(trn2inreg1),
    pub_si = as.logical(pub_si),
    pub_abs = as.logical(pub_abs),
    pub_ft = as.logical(pub_ft),
    is_match_protocol_sponsor_protocol_id = as.logical(is_match_protocol_sponsor_protocol_id),
    is_match_results_sponsor_protocol_id = as.logical(is_match_results_sponsor_protocol_id),
    is_title_matched = as.logical(is_title_matched),
    other = as.character(other),
    crossreg_manual_valid = as.logical(crossreg_manual_valid),
    validation_date = as.Date(validation_date, format = "%Y-%m-%d"),
    comment = as.character(comment),
    stringsAsFactors = FALSE
  )

  # Add the new row to the trn_trn table
  trn_trn <- rbind(trn_trn, new_row)

  return(trn_trn)
}

# Function for editing rows of trn_trn. Takes trn1 and trn2 as input, and updates the values that are passed as arguments to it.
# Checks in row that existing values are not overwritten by the default values of NA in function signature
update_trn_trn_row <- function(trn1, trn2, registry1 = NA, registry2 = NA,
                               trn1inreg2 = NA, trn2inreg1 = NA, pub_si = NA,
                               pub_abs = NA, pub_ft = NA, is_match_protocol_sponsor_protocol_id = NA, is_match_results_sponsor_protocol_id = NA, is_title_matched = NA,
                               other = NA, crossreg_manual_valid = NA,
                               validation_date = NA, comment = NA) {

  # Find the row index where trn1 and trn2 match
  row_index <- which(trn_trn$trn1 == trn1 & trn_trn$trn2 == trn2)

  # Retrieve existing values from the row
  existing_values <- trn_trn[row_index, ]

  # Update only the non-NA values provided as arguments
  existing_values$registry1 <- ifelse(!is.na(registry1), registry1, existing_values$registry1)
  existing_values$registry2 <- ifelse(!is.na(registry2), registry2, existing_values$registry2)
  existing_values$trn1inreg2 <- ifelse(!is.na(trn1inreg2), trn1inreg2, existing_values$trn1inreg2)
  existing_values$trn2inreg1 <- ifelse(!is.na(trn2inreg1), trn2inreg1, existing_values$trn2inreg1)
  existing_values$pub_si <- ifelse(!is.na(pub_si), pub_si, existing_values$pub_si)
  existing_values$pub_abs <- ifelse(!is.na(pub_abs), pub_abs, existing_values$pub_abs)
  existing_values$pub_ft <- ifelse(!is.na(pub_ft), pub_ft, existing_values$pub_ft)
  existing_values$is_match_protocol_sponsor_protocol_id <- ifelse(!is.na(is_match_protocol_sponsor_protocol_id), is_match_protocol_sponsor_protocol_id, existing_values$is_match_protocol_sponsor_protocol_id)
  existing_values$is_match_results_sponsor_protocol_id <- ifelse(!is.na(is_match_results_sponsor_protocol_id), is_match_results_sponsor_protocol_id, existing_values$is_match_results_sponsor_protocol_id)
  existing_values$is_title_matched <- ifelse(!is.na(is_title_matched), is_title_matched, existing_values$is_title_matched)
  existing_values$other <- ifelse(!is.na(other), other, existing_values$other)
  existing_values$crossreg_manual_valid <- ifelse(!is.na(crossreg_manual_valid), crossreg_manual_valid, existing_values$crossreg_manual_valid)
  existing_values$validation_date <- ifelse(!is.na(validation_date), as.Date(validation_date, format = "%Y-%m-%d"), existing_values$validation_date)
  existing_values$comment <- ifelse(!is.na(comment), comment, existing_values$comment)

  # Update the row in trn_trn
  trn_trn[row_index, ] <- existing_values

  # Return the updated trn_trn data frame
  return(trn_trn)
}

####################################################################################################################

## First we will populate the matches table using our registry data
# We will make a pair for every ID with any values in the trns_reg column.
# We will look up the mentioned TRN in the table to see if that TRN contains any mention of the original TRN.
# Before adding to the table, we will make sure that the current pair does not exist


# Iterate through each row of TRN_registry_data
for (i in 1:nrow(TRN_registry_data)) {

  current_id <- TRN_registry_data$id[i]
  trns_reg <- TRN_registry_data$trns_reg[i]

  # Skip rows where trns_reg is empty
  if (is.na(trns_reg) || trns_reg == "") {
    next
  }

  # Split trns_reg by semicolon to get individual trial IDs
  trns <- strsplit(trns_reg, ";")[[1]]

  # Iterate through each pair of trial IDs in trn_trn
  for (current_trn in trns) {

    # Ensure trn-trn does not capture self-references
    if (current_id != current_trn) {

      # Check if the pair already exists in trn_trn
      if (!any(trn_trn$trn1 == current_id & trn_trn$trn2 == current_trn)) {

        # The below checks whether trn1 is found in any of the trns_reg fields of the rows of TRN_registry_data in which
        # TRN_registry_data$id == trn2. Since multiple rows of TRN_registry_data could have id == trn2, each potentially
        # with a different trns_reg, it is necessary to first unlist using semicolons as dividers, and then to look at all these discrete strings
        # together. The same is done to check the converse (trn2inreg1).

        current_trn1inreg2 = current_id %in% unlist(lapply(strsplit(TRN_registry_data$trns_reg[TRN_registry_data$id == current_trn], ";"), unlist))
        current_trn2inreg1 = current_trn %in% unlist(lapply(strsplit(TRN_registry_data$trns_reg[TRN_registry_data$id == current_id], ";"), unlist))

        # Add new row to trn_trn if pairing is unique and not self-referential.
        trn_trn <- add_trn_trn_row(trn1 = current_id,
                                   trn2 = current_trn,
                                   registry1 = which_registry(current_id),
                                   registry2 = which_registry(current_trn),
                                   trn1inreg2 = current_trn1inreg2,
                                   trn2inreg1 = current_trn2inreg1)
      }
    }
  }
}

####################################################################################################################

# Now we integrate any connections made from the sponsor protocol number. If the pair is unique, add it in table and set
# sponsor connection boolean to TRUE. If the pair already exists, just update the boolean value.


# FIRST go through the protocol_sponsor_linked_trn

for (i in 1:nrow(TRN_registry_data)) {

  current_id <- TRN_registry_data$id[i]
  current_protocol_sponsor_linked_trn <- TRN_registry_data$protocol_sponsor_linked_trn[i]

  # Skip rows where sponsor_linked_trn is empty
  if (is.na(current_protocol_sponsor_linked_trn) || current_protocol_sponsor_linked_trn == "") {
    next
  }


  # Ensure trn-trn does not capture self-references
  if (current_id != current_protocol_sponsor_linked_trn) {

    # If the pair exists already, update the boolean is_match_sponsor_protocol ID to reflect new layer of connection
    if (any(trn_trn$trn1 == current_id & trn_trn$trn2 == current_protocol_sponsor_linked_trn)) {

      trn_trn <- update_trn_trn_row(trn1 = current_id,
                                    trn2 = current_protocol_sponsor_linked_trn,
                                    is_match_protocol_sponsor_protocol_id = TRUE)
    }
    # If the pairing is unique, add new row to table with is_match_sponsor_protocol_id initialized to TRUE
    else {
      trn_trn <- add_trn_trn_row(trn1 = current_id,
                                 trn2 = current_protocol_sponsor_linked_trn,
                                 registry1 = which_registry(current_id),
                                 registry2 = which_registry(current_protocol_sponsor_linked_trn),
                                 trn1inreg2 = NA,
                                 trn2inreg1 = NA,
                                 is_match_protocol_sponsor_protocol_id = TRUE)
    }
  }
}

# SECOND go through results_sponsor_linked_trn

for (i in 1:nrow(TRN_registry_data)) {

  current_id <- TRN_registry_data$id[i]
  current_results_sponsor_linked_trn <- TRN_registry_data$results_sponsor_linked_trn[i]

  # Skip rows where sponsor_linked_trn is empty
  if (is.na(current_results_sponsor_linked_trn) || current_results_sponsor_linked_trn == "") {
    next
  }


  # Ensure trn-trn does not capture self-references
  if (current_id != current_results_sponsor_linked_trn) {

    # If the pair exists already, update the boolean is_match_sponsor_protocol ID to reflect new layer of connection
    if (any(trn_trn$trn1 == current_id & trn_trn$trn2 == current_results_sponsor_linked_trn)) {

      trn_trn <- update_trn_trn_row(trn1 = current_id,
                                    trn2 = current_results_sponsor_linked_trn,
                                    is_match_results_sponsor_protocol_id = TRUE)
    }
    # If the pairing is unique, add new row to table with is_match_sponsor_protocol_id initialized to TRUE
    else {
      trn_trn <- add_trn_trn_row(trn1 = current_id,
                                 trn2 = current_results_sponsor_linked_trn,
                                 registry1 = which_registry(current_id),
                                 registry2 = which_registry(current_results_sponsor_linked_trn),
                                 trn1inreg2 = NA,
                                 trn2inreg1 = NA,
                                 is_match_results_sponsor_protocol_id = TRUE)
    }
  }
}


####################################################################################################################
# Now lets add matches made from the title matching algorithm. Will follow similar logic to steps above

for (i in 1:nrow(title_matches)) {
  current_trn1 = title_matches$id[i]
  current_trn2 = title_matches$euctr_id[i]

  # Eliminate all self-references
  if (current_trn1 != current_trn2) {

    # If the pair exists already, update the boolean is_match_sponsor_protocol ID to reflect new layer of connection
    if (any(trn_trn$trn1 == current_trn1 & trn_trn$trn2 == current_trn2)) {

      trn_trn <- update_trn_trn_row(trn1 = current_trn1,
                                    trn2 = current_trn2,
                                    is_title_matched = TRUE)
    }
    else {

      current_trn1inreg2 = current_trn1 %in% unlist(lapply(strsplit(TRN_registry_data$trns_reg[TRN_registry_data$id == current_trn2], ";"), unlist))
      current_trn2inreg1 = current_trn2 %in% unlist(lapply(strsplit(TRN_registry_data$trns_reg[TRN_registry_data$id == current_trn1], ";"), unlist))

      # Add new row to trn_trn if pairing is unique and not self-referential.
      trn_trn <- add_trn_trn_row(trn1 = current_trn1,
                                 trn2 = current_trn2,
                                 registry1 = which_registry(current_trn1),
                                 registry2 = which_registry(current_trn2),
                                 trn1inreg2 = current_trn1inreg2,
                                 trn2inreg1 = current_trn2inreg1,
                                 is_title_matched = TRUE)
      }

  }
}

####################################################################################################################
## Finally let's add matches from TRNs found in publications

# Iterate through publications table and add matches not found before, or update info on matches that already exist

for (i in 1:nrow(publications)) {

  # load variables for each row
  current_trn1 = publications$primary_IV_id[i]
  current_trns_si = publications$trns_si[i]
  current_trns_abs = publications$trns_abs[i]
  current_trns_ft = publications$trns_ft[i]

  # unlist TRNs from semicolon separated format for each row
  si_list =  strsplit(current_trns_si, ";")[[1]]
  abs_list = strsplit(current_trns_abs, ";")[[1]]
  ft_list = strsplit(current_trns_ft, ";")[[1]]

  ###### First go through SI TRNs, provided that the field is not NA or empty
  if (!is.na(current_trns_si) & current_trns_si != "") {
    for (current_trn2 in si_list) {

      # If the current trn2 is NA or empty, skip to next
      if(current_trn2 == "NA" | is.na(current_trn2) | current_trn2 == "") {
        next
      }

      # If the current trn2 is equal to trn1, skip to next (eliminate self references)
      if (current_trn1 == current_trn2) {
        next
      }
      else {

        # If the match already exists, just update with new boolean info
        if (any(trn_trn$trn1 == current_trn1 & trn_trn$trn2 == current_trn2)) {

          trn_trn <- update_trn_trn_row(trn1 = current_trn1,
                                        trn2 = current_trn2,
                                        pub_si = TRUE)
        }
        # If the match is unique, add new row with necessary info
        else {
          current_trn1inreg2 = current_trn1 %in% unlist(lapply(strsplit(TRN_registry_data$trns_reg[TRN_registry_data$id == current_trn2], ";"), unlist))
          current_trn2inreg1 = current_trn2 %in% unlist(lapply(strsplit(TRN_registry_data$trns_reg[TRN_registry_data$id == current_trn1], ";"), unlist))

          # Add new row to trn_trn if pairing is unique and not self-referential.
          trn_trn <- add_trn_trn_row(trn1 = current_trn1,
                                     trn2 = current_trn2,
                                     registry1 = which_registry(current_trn1),
                                     registry2 = which_registry(current_trn2),
                                     trn1inreg2 = current_trn1inreg2,
                                     trn2inreg1 = current_trn2inreg1,
                                     pub_si = TRUE)
        }
      }
    }
  }

  ###### Second go through abstract information. Similar logic to SI section
  if (!is.na(current_trns_abs) & current_trns_abs != "") {
    for (current_trn2 in abs_list) {

      # If the current trn2 is NA or empty, skip to next
      if(current_trn2 == "NA" | is.na(current_trn2) | current_trn2 == "") {
        next
      }

      # If the current trn2 is equal to trn1, skip to next (eliminate self references)
      if (current_trn1 == current_trn2) {
        next
      }
      else {

        # If the match already exists, just update with new boolean info
        if (any(trn_trn$trn1 == current_trn1 & trn_trn$trn2 == current_trn2)) {

          trn_trn <- update_trn_trn_row(trn1 = current_trn1,
                                        trn2 = current_trn2,
                                        pub_abs = TRUE)
        }
        # If the match is unique, add new row with necessary info
        else {
          current_trn1inreg2 = current_trn1 %in% unlist(lapply(strsplit(TRN_registry_data$trns_reg[TRN_registry_data$id == current_trn2], ";"), unlist))
          current_trn2inreg1 = current_trn2 %in% unlist(lapply(strsplit(TRN_registry_data$trns_reg[TRN_registry_data$id == current_trn1], ";"), unlist))

          # Add new row to trn_trn if pairing is unique and not self-referential.
          trn_trn <- add_trn_trn_row(trn1 = current_trn1,
                                     trn2 = current_trn2,
                                     registry1 = which_registry(current_trn1),
                                     registry2 = which_registry(current_trn2),
                                     trn1inreg2 = current_trn1inreg2,
                                     trn2inreg1 = current_trn2inreg1,
                                     pub_abs = TRUE)
        }
      }
    }
  }

  ###### LASTLY, go through full text information. Same logic as SI and abstract sections

  if (!is.na(current_trns_ft) & current_trns_ft != "") {
    for (current_trn2 in ft_list) {

      # If the current trn2 is NA or empty, skip to next
      if(current_trn2 == "NA" | is.na(current_trn2) | current_trn2 == "") {
        next
      }

      # If the current trn2 is equal to trn1, skip to next (eliminate self references)
      if (current_trn1 == current_trn2) {
        next
      }
      else {

        # If the match already exists, just update with new boolean info
        if (any(trn_trn$trn1 == current_trn1 & trn_trn$trn2 == current_trn2)) {

          trn_trn <- update_trn_trn_row(trn1 = current_trn1,
                                        trn2 = current_trn2,
                                        pub_ft = TRUE)
        }
        # If the match is unique, add new row with necessary info
        else {
          current_trn1inreg2 = current_trn1 %in% unlist(lapply(strsplit(TRN_registry_data$trns_reg[TRN_registry_data$id == current_trn2], ";"), unlist))
          current_trn2inreg1 = current_trn2 %in% unlist(lapply(strsplit(TRN_registry_data$trns_reg[TRN_registry_data$id == current_trn1], ";"), unlist))

          # Add new row to trn_trn if pairing is unique and not self-referential.
          trn_trn <- add_trn_trn_row(trn1 = current_trn1,
                                     trn2 = current_trn2,
                                     registry1 = which_registry(current_trn1),
                                     registry2 = which_registry(current_trn2),
                                     trn1inreg2 = current_trn1inreg2,
                                     trn2inreg1 = current_trn2inreg1,
                                     pub_ft = TRUE)
        }
      }
    }
  }
}


####################################################################################################################

## Let's pare down the resulting table and remove rows we're not interested in, including:
# All rows which have the same trn1 and trn2
# All rows which have NA as a value in either trn1 or trn2
# Any duplicate rows

# Remove rows where trn1 and trn2 are the same
trn_trn <- trn_trn[trn_trn$trn1 != trn_trn$trn2, ]

# Remove rows where trn1 or trn2 are NA or equal to the string "NA"
trn_trn <- trn_trn[!(is.na(trn_trn$trn1) | trn_trn$trn1 == "NA" | is.na(trn_trn$trn2) | trn_trn$trn2 == "NA"), ]

# Remove duplicate rows
trn_trn <- unique(trn_trn) # if this is doing anything it means something isn't working further up; we should only be adding unique pairings in the first place

