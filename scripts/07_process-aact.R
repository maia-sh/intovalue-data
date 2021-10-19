library(dplyr)
# library(readr)
# library(fs)
# library(assertr)
# library(ctregistries)

# source(here::here("scripts", "functions", "duration_days.R"))
source(here::here("scripts", "functions", "aact-functions.R"))

input_dir <- here::here("data", "raw", "registries", "ctgov")
output_dir <- here::here("data", "processed", "registries", "ctgov")

process_aact(input_dir, output_dir)
