## Script to build TRN-TRN table for manual proofing using publications table, title matching, and TRN(registry data) table

##########################################################

library(here)
library(fs)
library(lubridate)
library(tidyverse)
library(ctregistries)
library("readxl")
library(here)

dir_raw <- here("data", "raw")

# Load LATEST EU protocol data to check that EUCTR IDs resolve (not the same EU dump we used to build these tables, a more updated version)
# Will add flag to any EUCTR IDs in table that aren't found in protocol dump
eu_protocol_dump <-
  read_csv(path(dir_raw, "registries", "euctr", "euctr_euctr_dump-2024-09-07-092059.csv"))

# Load registry data
trn_registry_data <- read_rds("data/cross-registrations/trn-registry-data.rds")

# Load title matching data
title_matches <- read_rds("data/cross-registrations/title-matched-10.rds")

# Load the publications table once it's ready
publications <- read_rds("data/cross-registrations/trn-publications.rds")

# Get trials, limited to those meeting intovalue criteria
source(here::here("scripts", "functions", "filter-intovalue-criteria.R"))
trials <- read_iv_trials()

# Load list of trials removed from DRKS after 2022. Will add flag to any TRN pairing that contains the DRKS number removed (not the associated NCT)
# Also, I edited the original file to change the first column name to drks_id and second column name to nct_id

drks_removed <- read_excel("data/raw/registries/drks/drks-nicht-migrierte-ctgov-studien.xlsx")
drks_removed <- drks_removed |>
  rename(drks_id = `Ehemalige DRKS-ID der nicht migrierten Studie\r\nFormer DRKS ID of study not migrated`)

####################################################################################################################

## First build matches table from registry data table

trn_trn_longer <- trn_registry_data |>
  filter(!is.na(trns_reg), # filter rows where trns_reg is empty
         trns_reg != "") |>
  mutate(trn_split = strsplit(trns_reg, ";")) |> # Split trns_reg by semicolon to get individual trial IDs
  unnest_longer(trn_split) |> # unnest longer to allow pairwise comparisons
  filter(trn_split != id) |> # filter out self-references
  rename(trn1 = id,
         trn2 = trn_split) |>
  distinct(trn1, trn2, .keep_all = TRUE) |>
  relocate(trn1, trn2, everything()) |>
  # Confirm that trn1/trn2 pairs are not repeated
  # Note that the same trns may appear flipped, i.e., trn2/trn1
  assertr::assert_rows(assertr::col_concat, assertr::is_uniq, trn1, trn2)


# TRN pairings with registry information added
trn_registries <- trn_trn_longer |>
  rowwise() |>
  mutate(registry1 = which_registry(trn1),
         registry2 = which_registry(trn2))

# Add information about whether registries reference each other
trn_trn_tidy <- trn_registries |>

  # Check if the current trn1 is mentioned in the registry of the current trn2. If it is, there will be a
  # row in trn_registries where trn1 = current trn2 and trn2 = current trn1
  mutate(trn1inreg2 = trn1 %in% trn_registries$trn2[trn_registries$trn1 == trn2],

         # trn2inreg1 is TRUE by default. We only know about trn2 because it is listed in the trns_reg field of trn1.
         # We don't need to waste any computations checking if trn2 is in the registry of trn1. At this step, it's a given
         trn2inreg1 = TRUE
  ) |>
  select(trn1, trn2, registry1, registry2, trn1inreg2, trn2inreg1) |>
  ungroup()

####################################################################################################################

# Integrate any connections made from the sponsor protocol number. If the pair is unique, add it in table and set
# sponsor connection boolean to TRUE. If the pair already exists, just update the boolean value.

# FIRST go through the protocol_sponsor_linked_trn

# Isolate protocol sponsor protocol matches
trn_trn_protocols <- trn_registry_data |>
  filter(!is.na(protocol_sponsor_linked_trn), # filter out rows where protocol_sponsor_linked_trn is empty
         protocol_sponsor_linked_trn != "",
         id != protocol_sponsor_linked_trn # or a self-reference
  ) |>
  mutate(is_match_protocol_sponsor_protocol_id = TRUE) |>
  select(trn1 = id, trn2 = protocol_sponsor_linked_trn, is_match_protocol_sponsor_protocol_id) |>
  distinct(trn1, trn2, .keep_all = TRUE)

# Integrate protocol sponsor protocol matches to larger table
trn_trn_protocols_tidy <- trn_trn_tidy |>
  mutate(is_match_protocol_sponsor_protocol_id = NA) |>
  rows_upsert(trn_trn_protocols, by = c("trn1", "trn2"))


# SECOND go through results_sponsor_linked_trn

# Isolate results sponsor protocol matches
trn_trn_results <- trn_registry_data |>
  filter(!is.na(results_sponsor_linked_trn), # filter out rows where results_sponsor_linked_trn is empty
         results_sponsor_linked_trn != "",
         id != results_sponsor_linked_trn # or a self-reference
  ) |>
  mutate(is_match_results_sponsor_protocol_id = TRUE) |>
  select(trn1 = id, trn2 = results_sponsor_linked_trn, is_match_results_sponsor_protocol_id) |>
  distinct(trn1, trn2, .keep_all = TRUE)

# Integrate new results matches
trn_trn_results_tidy <- trn_trn_protocols_tidy |>
  mutate(is_match_results_sponsor_protocol_id = NA) |>
  rows_upsert(trn_trn_results, by = c("trn1", "trn2"))


####################################################################################################################
# Now add matches made from the title matching algorithm. Will follow similar logic to steps above

# Modify structure of title match table to make it easier to integrate with larger match table
trn_trn_titles <- title_matches |>
  filter(id != euctr_id) |>   # filter out self-references
  select(trn1 = id, trn2 = euctr_id, is_title_matched = has_crossreg_eudract_tm)

# Integrate title matched TRNs
trn_trn_titles_tidy <- trn_trn_results_tidy |>
  mutate(is_title_matched = NA) |>
  rows_upsert(trn_trn_titles, by = c("trn1", "trn2"))

####################################################################################################################
## Finally add matches from TRNs found in publications

# Unnest TRNs in pubs table
trn_trn_pubs_long <- publications |>
  rename(trn1 = primary_IV_id) |>
  mutate(trn_si_long = strsplit(trns_si, ";"),
         trn_abs_long = strsplit(trns_abs, ";"),
         trn_ft_long = strsplit(trns_ft, ";")) |> # Split trns_si by semicolon to get individual trial IDs
  unnest_longer(trn_si_long) |> # unnest longer to allow pairwise comparisons
  filter(trn_si_long != trn1 | is.na(trn_si_long)) |> # filter out self-references
  unnest_longer(trn_abs_long) |> # unnest longer to allow pairwise comparisons
  filter(trn_abs_long != trn1 | is.na(trn_abs_long)) |> # filter out self-references
  unnest_longer(trn_ft_long) |> # unnest longer to allow pairwise comparisons
  filter(trn_ft_long != trn1 | is.na(trn_ft_long)) |> # filter out self-references
  select(trn1, contains("long"))

# Identify unique TRN pairings from pubs fields, fill out boolean information to identify pub source of match
trn_trn_pubs <- trn_trn_pubs_long |>
  rowwise() |>
  mutate(trn2 = list(unique(c(trn_si_long, trn_abs_long, trn_ft_long)) |> na.omit())) |>
  unnest_longer(trn2) |>
  group_by(trn1, trn2) |>
  summarise(trn2_in_pub_si = if_else(any(trn2 == trn_si_long), TRUE, FALSE),
            trn2_in_pub_abs = if_else(any(trn2 == trn_abs_long), TRUE, FALSE),
            trn2_in_pub_ft = if_else(any(trn2 == trn_ft_long), TRUE, FALSE)) |>
  select(trn1, trn2, contains("pub")) |>
  ungroup()

# Integrate pub matches to larger match table
trn_trn_pubs_tidy <- trn_trn_titles_tidy |>
  mutate(trn2_in_pub_si = NA, trn2_in_pub_abs = NA, trn2_in_pub_ft = NA) |>
  rows_upsert(trn_trn_pubs, by = c("trn1", "trn2"))


# Add registry information, and update trn1inreg2 and trn2inreg1 for all rows in table

trn_trn_registries <- trn_trn_pubs_tidy |>
  rowwise() |>
  mutate(registry1 = case_when(is.na(registry1) ~ which_registry(trn1),
                               TRUE ~ registry1),
         registry2 = case_when(is.na(registry2) ~ which_registry(trn2),
                               TRUE ~ registry2)) |>
  mutate(trn1inreg2 = trn1 %in% trn_registries$trn2[trn_registries$trn1 == trn2],
         trn2inreg1 = trn2 %in% trn_registries$trn2[trn_registries$trn1 == trn1]) |>
  ungroup() |>
  relocate(contains("pub"), .after = trn2inreg1)


####################################################################################################################
## Adding so called 'meta booleans' to make it easier to assign priorities to rows for manual validation stage

trn_trn_registries <- trn_trn_registries |>
  mutate(at_least_one_EU = if_else((registry1 == "EudraCT" | registry2 == "EudraCT"), TRUE, FALSE),
         at_least_one_IV = if_else((trn1 %in% trials$id | trn2 %in% trials$id), TRUE, FALSE),
         at_least_one_pub = if_else(!is.na(trn2_in_pub_si) | !is.na(trn2_in_pub_abs) | !is.na(trn2_in_pub_ft), TRUE, FALSE),
         at_least_one_sponsor_match = if_else(!is.na(is_match_protocol_sponsor_protocol_id) |
                                                !is.na(is_match_results_sponsor_protocol_id), TRUE, FALSE))

####################################################################################################################
# Assigning priorities for manual checks using meta booleans and other columns

# Need to account for NA values here for non 'meta-booleans'
# Use coalesce() function: returns the first non-NA value in a list. Eg.) coalesce(NA, FALSE) == FALSE ; coalesce(NA, NA, 7) == 7


trn_trn_final_tidy <- trn_trn_registries |>
  ungroup() |> # why the ungroup here?
  mutate(priority = NA) |>
  mutate(priority = case_when(

    # Priority 1
    at_least_one_EU &
      at_least_one_IV &
      trn1inreg2 &
      trn2inreg1 &
      is.na(priority) ~ 1,

    # Priority 2
    at_least_one_EU &
      at_least_one_IV &
      (trn1inreg2 | trn2inreg1) &
      coalesce(is_title_matched, FALSE) &
      is.na(priority) ~ 2,

    # Priority 3
    at_least_one_EU &
      at_least_one_IV &
      (trn1inreg2 | trn2inreg1) &
      !coalesce(is_title_matched, FALSE) &
      is.na(priority) ~ 3,

    # Priority 4
    at_least_one_EU &
      at_least_one_IV &
      coalesce(is_title_matched, FALSE) &
      at_least_one_sponsor_match &
      is.na(priority) ~ 4,

    # Priority 5
    at_least_one_EU &
      at_least_one_IV &
      coalesce(is_title_matched, FALSE) &
      is.na(priority) ~ 5,

    # Priority 6
    at_least_one_EU &
      at_least_one_IV &
      at_least_one_pub &
      is.na(priority) ~ 6,

    # Priority 7
    at_least_one_EU &
      at_least_one_IV &
      at_least_one_sponsor_match &
      is.na(priority)~ 7,

    # Everything else
    .default = 8
  ))

# Add flag to TRN pairings where at least one of trn1 or trn2 contains a DRKS number mentioned in the deleted numbers table
trn_trn_final_tidy <- trn_trn_final_tidy |>
  mutate(drks_removed = if_else(trn1 %in% drks_removed$drks_id | trn2 %in% drks_removed$drks_id, TRUE, FALSE)) |>
  mutate(euctr_id_not_in_euctr = case_when(

    # TRUE if either of the TRNs is an EUCTR ID and NOT in protocol dump
    (registry1 == "EudraCT" & !(trn1 %in% eu_protocol_dump$eudract_number)) | (!(trn2 %in% eu_protocol_dump$eudract_number) & registry2 == "EudraCT") ~ TRUE,

    # FALSE if either of the TRNs is an EUCTR ID and both are in protocol dump
    (registry1 == "EudraCT" & (trn1 %in% eu_protocol_dump$eudract_number)) | ((trn2 %in% eu_protocol_dump$eudract_number) & registry2 == "EudraCT") ~ FALSE,

    # NA if neither of the TRNs is an EUCTR ID
    registry1 != "EudraCT" & registry2 != "EudraCT" ~ NA
  ))

# save
saveRDS(trn_trn_final_tidy, "data/cross-registrations/trn_trn.rds")
