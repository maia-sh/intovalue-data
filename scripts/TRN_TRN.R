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

    #if trn_trn is empty, just add the first pair since the code for checking for existing pairs breaks with an empty table
    if (nrow(trn_trn) == 0) {

      # Append a new row to trn_trn
      trn_trn <- rbind(
          trn_trn,
          data.frame(
          trn1 = current_id,
          trn2 = trials[j],
          registry1 = which_registry(current_id),
          registry2 = which_registry(trials[j]),
          trn1inreg2 = current_id %in% unlist(strsplit(as.character(TRN_registry_data$trns_reg[i]), ";")), # This is not doing what we think it is
          trn2inreg1 = trials[j] %in% unlist(strsplit(as.character(TRN_registry_data$trns_reg[i]), ";"))
        )
      )
    }
    else {

      #check for prior existence of pair
      if (!(trn_trn$trn1 %in% c(id, trials[j]) & trn_trn$trn2 %in% c(id, trials[j])) & # Is this doing what I think it is?
        !(trn_trn$trn2 %in% c(id, trials[j]) & trn_trn$trn1 %in% c(id, trials[j]))) {

        # Append a new row to trn_trn
        trn_trn <- rbind(
            trn_trn,
            data.frame(
            trn1 = current_id,
            trn2 = trials[j],
            registry1 = which_registry(current_id),
            registry2 = which_registry(trials[j]),
            trn1inreg2 = current_id %in% unlist(strsplit(as.character(TRN_registry_data$trns_reg[i]), ";")),
            trn2inreg1 = trials[j] %in% unlist(strsplit(as.character(TRN_registry_data$trns_reg[i]), ";"))
          )
        )
      }
    }
  }
}


## Let's pare down the resulting table and remove rows we're not interested in, including:
# All rows which have the same trn1 and trn2
# All rows which have NA as a value in either trn1 or trn2
# Any duplicate rows

# Remove rows where trn1 and trn2 are the same
trn_trn <- trn_trn[trn_trn$trn1 != trn_trn$trn2, ]

# Remove rows where trn1 or trn2 are NA or equal to the string "NA"
trn_trn <- trn_trn[!(is.na(trn_trn$trn1) | trn_trn$trn1 == "NA" | is.na(trn_trn$trn2) | trn_trn$trn2 == "NA"), ]

# Remove duplicate rows
trn_trn <- unique(trn_trn)





## Here we will use the publications table when it is ready!

