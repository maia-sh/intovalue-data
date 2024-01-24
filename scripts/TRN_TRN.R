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

TRN_registry_data = read_rds("TRN(registry data).rds")
# this is where we will load the publications table once its ready

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
# Before adding to the table, we will make sure that the current pair does not exist, both forwards and backwards.


####################################################################################################################


add_connection <- function(trn1, trn2, trn1inreg2, trn2inreg1, connection_table) {
  # Check if the connection already exists
  if (!(trn1 %in% connection_table$TRN1 & trn2 %in% connection_table$TRN2) &&
      !(trn2 %in% connection_table$TRN1 & trn1 %in% connection_table$TRN2)) {
    # Add the new connection to the table
    connection_table <- rbind(connection_table, c(trn1, trn2, trn1inreg2, trn2inreg1))
  }
  connection_table
}

# Iterate over rows in TRN_registry_data
for (i in 1:nrow(TRN_registry_data)) {
  # Extract information from the current row
  current_id <- TRN_registry_data[i, "id"]
  trns_reg <- strsplit(as.character(TRN_registry_data[i, "trns_reg"]), ";")[[1]]

  # Iterate over pairs in trns_reg
  for (j in 1:length(trns_reg)) {
    trn1 <- current_id
    trn2 <- trns_reg[j]

    # Check if trn2 is in trn1's trns_reg
    trn1inreg2 <- trn2 %in% strsplit(as.character(TRN_registry_data[TRN_registry_data$id == trn1, "trns_reg"]), ";")[[1]]

    # Check if trn1 is in trn2's trns_reg
    trn2inreg1 <- trn1 %in% strsplit(as.character(TRN_registry_data[TRN_registry_data$id == trn2, "trns_reg"]), ";")[[1]]

    # Add the connection to TRN_TRN
    TRN_TRN <- add_connection(trn1, trn2, trn1inreg2, trn2inreg1, TRN_TRN)
  }
}


## Then we will populate the table using our publications data.

