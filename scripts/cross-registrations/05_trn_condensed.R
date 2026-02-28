# Script to condense duplicate entries in trn_trn table to make manual reconciliation easier.

library(here)
library(fs)
library(lubridate)
library(tidyverse)
library(ctregistries)
library("readxl")
library(here)

dir_raw <- here("data", "raw")

# Load long trn_trn table
trn_trn <- read_rds("data/cross-registrations/trn_trn.rds")

# Load LATEST EU protocol data to check that EUCTR IDs resolve (not the same EU dump we used to build these tables, a more updated version)
# Will add flag to any EUCTR IDs in table that aren't found in protocol dump
eu_protocol_dump <-
  read_csv(path(dir_raw, "registries", "euctr", "euctr_euctr_dump-2024-09-07-092059.csv"))

# Load list of trials removed from DRKS after 2022. Will add flag to any TRN pairing that contains the DRKS number removed (not the associated NCT)
# Also, I edited the original file to change the first column name to drks_id and second column name to nct_id

drks_removed <- read_excel("data/raw/registries/drks/drks-nicht-migrierte-ctgov-studien.xlsx") |>
  rename(drks_id = `Ehemalige DRKS-ID der nicht migrierten Studie\r\nFormer DRKS ID of study not migrated`)

# Drop unnecessary columns, add new publication booleans
trn_trn_no_regs <- trn_trn |>
  select(-registry1, -registry2, -drks_removed, -euctr_id_not_in_euctr)

# Create a unique identifier for pairs, which allows us to identify duplicate pairs
trn_trn_pair_id <- trn_trn_no_regs |>
  rowwise() |>
  mutate(pair_id = paste(sort(c(trn1, trn2)), collapse = "_")) |>
  ungroup()

# get duplicates and only work on those to ensure the rest of data stay in original state
# and this also speeds up processig a tiny bit
trn_trn_dupes <- janitor::get_dupes(trn_trn_pair_id, pair_id) |> # janitor is a helpful library for this
  group_by(pair_id) |>
  arrange(trn1) |> # arranging by trn1 gives one of the registries priority, which is nicer than having pairs randomly having e.g. NCT trns first or EUCT trns first
  mutate(rank = 1:n()) |> # this gives rank to the first trn in alphabetical order
  ungroup()

#### run your code with trn_trn_dupes instead of trn_trn_pair_id to allow for direct comparisons here and below!
# this function renames 1s to 2s and vice versa. There may be a better method, but I worked for speed here
rerank <- function(string) {
  string |>
    stringr::str_replace_all("1", "!!") |>
    stringr::str_replace_all("2", "1") |>
    stringr::str_replace_all("!!", "2")
}

rank1 <- trn_trn_dupes |>
  filter(rank == 1) |>
  select(pair_id, trn1, trn1inreg2, contains("trn2_"), contains("is_"),
         contains("at_least"))

rank2 <- trn_trn_dupes |>
  filter(rank == 2) |>
  select(pair_id, trn1, trn1inreg2, contains("trn2_"), contains("is_"),
         contains("at_least")) |>
  rename_with(rerank)

ranks_merged <- rank1 |>
  left_join(rank2, by = "pair_id") |>
  # this join adds .x and .y to the column names that are found in both tables and are not "pair_id"
  mutate(is_match_protocol_sponsor_protocol_id =
           coalesce(is_match_protocol_sponsor_protocol_id.x,
                    is_match_protocol_sponsor_protocol_id.y),
         is_match_results_sponsor_protocol_id =
           coalesce(is_match_results_sponsor_protocol_id.x,
                    is_match_results_sponsor_protocol_id.y),
         is_title_matched =
           coalesce(is_title_matched.x, is_title_matched.y),
         at_least_one_pub = at_least_one_pub.x | at_least_one_pub.y,
         at_least_one_sponsor_match = at_least_one_sponsor_match.x |
           at_least_one_sponsor_match.y,
         at_least_one_EU = at_least_one_EU.x | at_least_one_EU.y ,
         at_least_one_IV = at_least_one_IV.x | at_least_one_IV.y) |>
  select(pair_id, trn1, trn2, trn1inreg2, trn2inreg1,
         contains("trn1_"), contains("trn2_"), is_match_protocol_sponsor_protocol_id,
         is_match_results_sponsor_protocol_id, is_title_matched,
         at_least_one_pub,
         at_least_one_sponsor_match, at_least_one_EU, at_least_one_IV)

trn_no_dupes_ranks <- trn_trn_pair_id |>
  filter(!pair_id %in% ranks_merged$pair_id) |> # remove old dupes first
  bind_rows(ranks_merged) |> # add deduplicated trns
  arrange(trn1, trn2)


# Reassign priorities based on Delwen's proposal:
trn_priorities <- trn_no_dupes_ranks |>
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
      is.na(priority) ~ 2,

    # Priority 3
    at_least_one_EU &
      at_least_one_IV &
      is_title_matched &
      is.na(priority) ~ 3,


    # Priority 4
    at_least_one_EU &
      at_least_one_IV &
      at_least_one_pub &
      is.na(priority) ~ 4,

    # Priority 5
    at_least_one_EU &
      at_least_one_IV &
      at_least_one_sponsor_match &
      is.na(priority) ~ 5,

    .default = 7
  ))

# Rearrange columns to make manual checks easiest
trn_manual_checks <- trn_priorities |>
  relocate(trn1_in_pub_si, trn1_in_pub_abs, trn1_in_pub_ft, .before = trn2_in_pub_si) |>
  select(-pair_id)

# Add flag to TRN pairings where at least one of trn1 or trn2 contains a DRKS number mentioned in the deleted numbers table
trn_manual_checks <- trn_manual_checks |>
  mutate(drks_removed = if_else(trn1 %in% drks_removed$drks_id | trn2 %in% drks_removed$drks_id, TRUE, FALSE)) |>
  mutate(euctr_id_in_euctr = if_else((trn1 %in% eu_protocol_dump$eudract_number) | (trn2 %in% eu_protocol_dump$eudract_number), TRUE, FALSE))

# Save finished product
saveRDS(trn_manual_checks, "data/cross-registrations/trn_manual_checks.rds")
