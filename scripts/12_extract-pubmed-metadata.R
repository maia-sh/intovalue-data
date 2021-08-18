library(dplyr)
library(readr)
library(fs)
library(here)
library(tidypubmed)

source(here("R", "functions", "extract_pubmed.R"))

dir <- here("data", "processed", "pubmed")

pubmed_xmls <- dir_ls(here("data", "raw", "pubmed"))

pubmed_main <-
  pubmed_xmls %>%
  purrr::map_dfr(extract_pubmed, datatype = "main", quiet = FALSE) %>%
  rename(pmcid = pmc) %>%
  mutate(doi = tolower(doi))

write_rds(pubmed_main, path(dir, "pubmed-main", ext = "rds"))
