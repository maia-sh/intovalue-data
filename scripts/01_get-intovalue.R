library(dplyr)
library(fs)
library(readr)

dir <- dir_create(here::here("data", "raw"))

read_csv("https://zenodo.org/record/5141343/files/iv_main_dataset.csv?download=1") %>%

  # Shorten names for publication ids
  rename(
    doi = publication_doi,
    pmid = publication_pmid,
    url = publication_url
  ) %>%

  # Fix some publication ids mistakenly linked to errata rather than main pub
  mutate(pmid = if_else(doi == "10.3238/arztebl.2016.0347", 27294814, pmid, missing = pmid)) %>%

  rows_update(
    tibble(
      id = "DRKS00004776",
      iv_version = 2,
      pmid = 31086958,
      doi = "10.1093/jac/dkz203",
      url = "https://pubmed.ncbi.nlm.nih.gov/31086958/"
    ),
    by = c("id", "iv_version")
  ) %>%

  write_csv(path(dir, "intovalue.csv"))
