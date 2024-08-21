# New registry data is gathered for cross-registrations project in 2024
# As we don't have a good versioning system for data, I make a new script
# Note: Change to DRKS website and hence scraper

library(dplyr)
library(purrr)
library(stringr)
library(readr)

# ClinicalTrials.gov ------------------------------------------------------

# devtools::install_github("maia-sh/aactr")
library(aactr)

dir_raw_ctgov <- here::here("data", "raw", "registries", "ctgov_2024-08")
dir_processed_ctgov <- here::here("data", "processed", "registries", "ctgov_2024-08")

# Get clinicaltrial.gov trns
ct_trns <-
  readr::read_rds(here::here("data", "processed", "trn", "trn-all.rds")) %>%
  filter(registry == "ClinicalTrials.gov") %>%
  pull(trn)

# Specify aact username
AACT_USER <- "respmetrics"

download_aact(ids = ct_trns, dir = dir_raw, user = AACT_USER)

process_aact(dir_raw, dir_processed)

# Note: some warnings about date parsing produced and should be looked into before using the data


# DRKS --------------------------------------------------------------------

# devtools::install_github("quest-bih/dRks")
library(dRks)

dir_raw_drks <- fs::dir_create(here::here("data", "raw", "registries", "drks_2024-08"))
dir_processed_drks <- fs::dir_create(here::here("data", "processed", "registries", "drks_2024-08"))

# Get drks trns
drks_trns <-
  readr::read_rds(here::here("data", "processed", "trn", "trn-all.rds")) %>%
  filter(registry == "DRKS") %>%
  pull(trn)

# Download drks records
purrr::walk(drks_trns, dRks::download_drks, dir = dir_raw_drks)

# Log query date
loggit::set_logfile(here::here("queries.log"))
loggit::loggit("INFO", "DRKS")

# Parse all records in directory
drks_htmls <- fs::dir_ls(dir_raw_drks)

## Parse secondary ids ----------------------------------------------------

drks_ids <-
  drks_htmls |>
  map_dfr(parse_drks_ids) |>
  ctregistries::mutate_trn_registry(id) |>

  # Several ISRCTN are misformatted so capture here
  mutate(

    trn = if_else(
      str_detect(id, "\\d{8}[[:blank:][:punct:]]*ISRCTN"),
      str_c("ISRCTN", str_extract(id, "\\d{8}")),
      trn
    ),

    registry = if_else(
      str_detect(id, "\\d{8}[[:blank:][:punct:]]*ISRCTN"),
      "ISRCTN",
      registry
    )
  ) |>

  # Clean trns and collapse EudraCT entries
  mutate(
    raw_trn = trn,
    trn = purrr::map_chr(raw_trn, ctregistries::clean_trn),
    id_type = if_else(stringr::str_detect(id_type, "EudraCT"),"EudraCT", id_type)
  )

write_rds(drks_ids, fs::path(dir_processed_drks, "drks-ids", ext = "rds"))

drks_crossreg <-
  drks_ids |>
  filter(!is.na(trn)) |>
  select(drks_id, crossreg_registry = registry, crossreg_trn = trn)

write_rds(drks_crossreg, fs::path(dir_processed_drks, "drks-crossreg", ext = "rds"))

# Note: additional processing can be ported from `08_process-drks.R`
