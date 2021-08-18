# Create df of retrieved pubmed and pdfs (doi and pmid), with NA if no pmid
# Also add whether source of pdf if doi or pmid (TRUE/FALSE only)
# Use xml versions of pdfs

library(dplyr)
library(readr)
library(fs)
library(here)
library(stringr)

intovalue <- read_csv(here("data", "raw", "intovalue.csv"))

# Prepare paths
dir_pubmed <- here("data", "raw", "pubmed")
dir_doi_xml <- here("data", "raw", "fulltext", "doi", "xml")
dir_pmid_xml <- here("data", "raw", "fulltext", "pmid", "xml")

dir_pubmed_processed <- dir_create(here("data", "processed", "pubmed"))

# List retrieved records
pubmed_retrieved <-
  dir_pubmed %>%
  dir_ls() %>%
  path_file() %>%
  path_ext_remove()

ft_doi_retrieved <-
  dir_doi_xml %>%
  dir_ls() %>%
  path_file() %>%
  str_remove(".tei.xml$") %>%
  str_replace_all("\\+", "/")

ft_pmid_retrieved <-
  dir_pmid_xml %>%
  dir_ls() %>%
  path_file() %>%
  str_remove(".tei.xml$") %>%
  str_replace_all("\\+", "/")

pubmed_ft_retrieved <-
  intovalue %>%
  select(id, doi, pmid) %>%
  mutate(
    has_pubmed = case_when(
      is.na(pmid) ~ NA,
      pmid %in% pubmed_retrieved ~ TRUE,
      TRUE ~ FALSE
    ),

    has_ft = case_when(
      is.na(pmid) ~ NA,
      (doi %in% ft_doi_retrieved) | (pmid %in% ft_pmid_retrieved) ~ TRUE,
      TRUE ~ FALSE
    ),

    ft_source = case_when(
      doi %in% ft_doi_retrieved ~ "doi",
      pmid %in% ft_pmid_retrieved ~ "pmid",
      TRUE ~ NA_character_
    )
    # ft_doi = if_else(doi %in% ft_doi_retrieved, TRUE, FALSE),
    # ft_pmid = if_else(pmid %in% ft_pmid_retrieved, TRUE, FALSE),
  ) %>%

  # Remove duplicates due to intovalue versions
  distinct()

write_rds(pubmed_ft_retrieved, path(dir_pubmed_processed, "pubmed-ft-retrieved.rds"))

