library(dplyr)
library(purrr)

source(here::here("R", "functions", "drks-functions.R"))

dir <- fs::dir_create(here::here("data", "raw", "registries", "drks"))

# Get drks trns
drks_trns <-
  readr::read_rds(here::here("data", "processed", "trn", "trn-all.rds")) %>%
  filter(registry == "DRKS") %>%
  pull(trn)

# Download drks records
purrr::walk(drks_trns, download_drks, dir = dir)

# Log query date
loggit::set_logfile(here::here("queries.log"))
loggit::loggit("INFO", "DRKS")
