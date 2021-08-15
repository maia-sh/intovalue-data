library(dplyr)
library(fs)
library(readr)

# Get data from other repositories, assuming both repositories in same parent directory
dir_repositories <- path_norm(path_wd(".."))
dir <- path(dir_repositories, "reg-pub-link")

dir_raw <- dir_create(here::here("data", "raw"))

# IntoValue
read_rds(path(dir, "data", "processed", "trials.rds")) %>%
  write_csv(path(dir_raw, "intovalue.csv"))

# Cross-registrations
read_rds(path(dir, "data", "processed", "cross-registrations.rds")) %>%
  write_csv(path(dir_raw, "cross-registrations.csv"))
