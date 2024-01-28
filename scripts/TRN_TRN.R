## Script to build TRN-TRN table for manual proofing using publications table and TRN(registry data table)

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

# This is where we will load the publications table once its ready

## Build our empty TRN-TRN table

TRN_TRN = data.frame(
  TRN1 = numeric(),
  TRN2 = numeric(),
  registry1 = character(),
  registry2 = character(),
  trn1inreg2 = logical(),
  trn2inreg1 = logical(),
  pub_si = logical(),
  pub_abs = logical(),
  pub_ft = logical(),
  other = character(),
  crossreg_manual_valid = logical(),
  validation_date = as.Date(character(), format = "%Y-%m-%d"),
  comment = character(),
  stringsAsFactors = FALSE
)

## First we will populate the matches table using our registry data
# We will make a pair for every ID with any values in the trns_reg column.
# We will look up the mentioned TRN in the table to see if that TRN contains any mention of the original TRN.
# Before adding to the table, we will make sure that the current pair does not exist


####################################################################################################################

# Iterate through rows of TRN_registry_data

for (i in seq_len(nrow(TRN_registry_data))) {

  # Extract the current main ID we are linking through
  current_id <- TRN_registry_data$id[i]

   # Extract all linked trns
  trials <- unlist(strsplit(as.character(TRN_registry_data$trns_reg[i]), ";"))

  # Iterate through unique pairs of trials
  for (j in seq_along(trials)) {

    #if TRN_TRN is empty, just add the first pair since the code for checking for existing pairs breaks with an empty table
    if (nrow(TRN_TRN) == 0) {

      # Get registry information for TRNs using Maia's function
      registry_info1 <- mutate_trn_registry(data.frame(id = current_id, stringsAsFactors = FALSE), id)
      registry_info2 <- mutate_trn_registry(data.frame(id = trials[j], stringsAsFactors = FALSE), id)

      # Append a new row to TRN_TRN
      TRN_TRN <- rbind(
          TRN_TRN,
          data.frame(
          TRN1 = current_id,
          TRN2 = trials[j],
          registry1 = registry_info1$registry,
          registry2 = registry_info2$registry,
          trn1inreg2 = current_id %in% unlist(strsplit(as.character(TRN_registry_data$trns_reg[i]), ";")),
          trn2inreg1 = trials[j] %in% unlist(strsplit(as.character(TRN_registry_data$trns_reg[i]), ";"))
        )
      )
    }
    else {

      #check for prior existence of pair
      if (!(TRN_TRN$TRN1 %in% c(id, trials[j]) & TRN_TRN$TRN2 %in% c(id, trials[j])) &
        !(TRN_TRN$TRN2 %in% c(id, trials[j]) & TRN_TRN$TRN1 %in% c(id, trials[j]))) {

        # Get registry information for TRNs using Maia's function
        registry_info1 <- mutate_trn_registry(data.frame(id = current_id, stringsAsFactors = FALSE), id)
        registry_info2 <- mutate_trn_registry(data.frame(id = trials[j], stringsAsFactors = FALSE), id)

        # Append a new row to TRN_TRN
        TRN_TRN <- rbind(
            TRN_TRN,
            data.frame(
            TRN1 = current_id,
            TRN2 = trials[j],
            registry1 = registry_info1$registry,
            registry2 = registry_info2$registry,
            trn1inreg2 = current_id %in% unlist(strsplit(as.character(TRN_registry_data$trns_reg[i]), ";")),
            trn2inreg1 = trials[j] %in% unlist(strsplit(as.character(TRN_registry_data$trns_reg[i]), ";"))
          )
        )
      }
    }
  }
}


## Here we will use the publications table when it is ready!

