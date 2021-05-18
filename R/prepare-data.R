library(dplyr)
library(fs)
library(readr)

# Get data from other repositories, assuming both repositories in same parent directory
dir_repositories <- path_norm(path_wd(".."))
dir <- path(dir_repositories, "reg-pub-link")

read_rds(path(dir, "data", "processed", "trials.rds")) %>%
  write_csv(here::here("data", "intovalue.csv"))
