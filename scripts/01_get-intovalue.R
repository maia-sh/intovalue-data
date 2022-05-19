library(dplyr)
library(fs)
library(readr)

dir <- dir_create(here::here("data", "raw"))

intovalue_raw <-
  read_csv("https://zenodo.org/record/5141343/files/iv_main_dataset.csv?download=1")

intovalue <-

  intovalue_raw %>%

  # Shorten names for publication ids
  rename(
    doi = publication_doi,
    pmid = publication_pmid,
    url = publication_url
  ) %>%

  # Fix some publication ids mistakenly linked to errata rather than main pub
  mutate(pmid = if_else(doi == "10.3238/arztebl.2016.0347", 27294814, pmid, missing = pmid)) %>%

  rows_update(tibble(
    id = "DRKS00004776",
    iv_version = 2,
    pmid = 31086958,
    doi = "10.1093/jac/dkz203",
    url = "https://pubmed.ncbi.nlm.nih.gov/31086958/"
  ), by = c("id", "iv_version")) %>%

  # 2022-01-11: NCT02632292 is led by Lübeck/Schleswig-Holstein and NOT Berlin
  mutate(lead_cities = if_else(id == "NCT02632292", "Schleswig-Holstein", lead_cities)) %>%

  # 2022-02-17: NCT01338922 had lead_city NA (IV1) and Schleswig-Holstein (IV2) -> checked and it's Schleswig-Holstein
  mutate(lead_cities = if_else(id == "NCT01338922", "Schleswig-Holstein", lead_cities)) %>%

  # 2022-05-19: NCT01503372 has publication (found incidentally during trackvalue crossreg checks)
  rows_update(tibble(
    id = "NCT01503372",
    pmid = 34741530,
    doi = "10.1002/ijc.33864",
    url = "https://pubmed.ncbi.nlm.nih.gov/34741530/",
    identification_step = "Cleaning"
  ), by = "id") %>%

  # 2022-05-19: NCT01984788 has publication url (letter, no abstract) but no doi/pmid
  rows_update(tibble(
    id = "NCT01984788",
    doi = "10.1016/j.jaci.2016.03.043",
    pmid = 27302552,
  ), by = "id") %>%

  # 2022-05-19: Deduplicate cross-registrations, prefering ctgov

  # NCT02071615/DRKS00005219
  filter(id != "DRKS00005219") %>%

  # NCT01703273/DRKS00004195 (only drks has charite lead, so trial won't be in trackvalue)
  filter(id != "DRKS00004195")

write_csv(intovalue, path(dir, "intovalue.csv"))
