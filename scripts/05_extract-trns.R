library(dplyr)
library(readr)
library(fs)
library(here)
# renv::install("maia-sh/ctregistries")
library(ctregistries)
# renv::install("maia-sh/tidypubmed")
library(tidypubmed)

source(here("scripts", "functions", "extract_pubmed.R"))
source(here("scripts", "functions", "get_grobid_ft_trn.R"))

intovalue <- read_csv(here("data", "raw", "intovalue.csv"))
pubmed_ft_retrieved <- read_rds(here("data", "processed", "pubmed", "pubmed-ft-retrieved.rds"))

# Prepare directory paths
dir_pubmed <- dir_create(here("data", "processed", "pubmed"))
dir_trn <- dir_create(here("data", "processed", "trn"))

# Get all unique pmid/doi combinations (for consistent pub ids as pmids across trns)
pmids_dois <-
  intovalue %>%
  distinct(pmid, doi) %>%
  tidyr::drop_na(pmid, doi)

# Extract TRNs from: PubMed secondary identifier, PubMed abstract, and PDF full-text
pubmed_xmls <- dir_ls(here("data", "raw", "pubmed"))


# Secondary identifier ----------------------------------------------------

si <-
  pubmed_xmls %>%
  purrr::map_dfr(extract_pubmed, datatype = "databanks", quiet = FALSE) %>%
  ctregistries::mutate_trn_registry(accession_number)

write_rds(si, path(dir_pubmed, "pubmed-si.rds"))
# si <- read_rds(path(dir_pubmed, "pubmed-si.rds"))

# Visually inspect mismatching trns and registries
# 1 trn is eudract with preceeding letters (detected, preceeding letter removed)
# 1 trn is eudract with country code (detected, country code removed)
# 5 trns are incorrectly formatted drks with no preceding "drks" (n = 4) or only "s" (n = 1) (not detected)
# 3 accession numbers are figshare
# 1 registry is attributed to genbank but drks
# 1 registry is attributed to genbank but ct.gov
# For now leave and later could discuss/analyze,
# Or, combine databank/accession_number and check for trn match
si_trn_mismatches <-
  si %>%
  filter(!accession_number %in% trn |
           !databank %in% registry)
# write_rds(si_trn_mismatches, path_wd("data", "processed", "si-trn-mismatches", ext = "rds"))

si <-
  si %>%
  tidyr::drop_na(trn) %>%
  select(pmid, registry, trn_detected = trn) %>%
  distinct() %>%
  group_by(pmid) %>%
  mutate(n_detected = row_number()) %>%
  ungroup() %>%
  mutate(
    source = "secondary_id",
    trn_cleaned = purrr::map_chr(trn_detected, ctregistries::clean_trn)
  ) %>%
  left_join(pmids_dois, by  = "pmid")

write_rds(si, path(dir_trn, "trn-si.rds"))
# si <- read_rds(path(dir_trn, "trn-si.rds"))

# Abstract ----------------------------------------------------------------

abs <-
  pubmed_xmls %>%
  purrr::map_dfr(extract_pubmed, datatype = "abstract", quiet = FALSE) %>%
  ctregistries::mutate_trn_registry(abstract)

write_rds(abs, path(dir_pubmed, "pubmed-abstract.rds"))
# abs <- read_rds(path(dir_pubmed, "pubmed-abstract.rds"))

abs <-
  abs %>%
  tidyr::drop_na(trn) %>%
  distinct(pmid, registry, trn_detected = trn) %>%
  group_by(pmid) %>%
  mutate(n_detected = row_number()) %>%
  ungroup() %>%
  mutate(
    source = "abstract",
    trn_cleaned = purrr::map_chr(trn_detected, ctregistries::clean_trn)
  ) %>%
  left_join(pmids_dois, by  = "pmid")

write_rds(abs, path(dir_trn, "trn-abstract.rds"))
# abs <- read_rds(path(dir_trn, "trn-abstract.rds"))

# Full-text DOI -----------------------------------------------------------

ft_doi_xmls <-
  dir_ls(here("data", "raw", "fulltext", "doi", "xml"))

ft_doi <-
  ft_doi_xmls %>%
  purrr::map_dfr(get_grobid_ft_trn) %>%
  select(-pmid) %>%
  rename(trn_detected = trn, n_detected = n) %>%
  mutate(
    source = "ft",
    trn_cleaned = purrr::map_chr(trn_detected, ctregistries::clean_trn)
  ) %>%
  left_join(pmids_dois, by  = "doi")

write_rds(ft_doi, path(dir_trn, "trn-ft-doi.rds"))
# ft_doi <- read_rds(path(dir_trn, "trn-ft-doi.rds"))

# Full-text PMID ----------------------------------------------------------

ft_pmid_xmls <-
  dir_ls(here("data", "raw", "fulltext", "pmid", "xml"))

ft_pmid <-
  ft_pmid_xmls %>%
  purrr::map_dfr(get_grobid_ft_trn) %>%
  select(-doi) %>%
  rename(trn_detected = trn, n_detected = n) %>%
  mutate(
    source = "ft",
    trn_cleaned = purrr::map_chr(trn_detected, ctregistries::clean_trn)
  ) %>%
  left_join(pmids_dois, by  = "pmid")

write_rds(ft_pmid, path(dir_trn, "trn-ft-pmid.rds"))
# ft_pmid <- read_rds(path(dir_trn, "trn-ft-pmid.rds"))

# Note: Could add whether typo and dupe after cleaning
# mutate(has_trn_typo = if_else(trn_detected != trn_cleaned, TRUE, FALSE)) %>%
# add_count(pmid, trn_cleaned, source)


# Combine reported TRNs (secondary id, abstract, full-text) ---------------

trn_combined <-
  bind_rows(si, abs, ft_doi, ft_pmid) %>%

  distinct(pmid, doi, trn = trn_cleaned, registry, source) %>%

  # All records should have a trn
  assertr::assert(assertr::not_na, trn)

write_rds(trn_combined, path(dir_trn, "trn-reported-long.rds"))
# trn_combined <- read_rds(path(dir_trn, "trn-reported-long.rds"))

# Pivot wider to for one row per TRN with sources as columns --------------

trn_combined <-
  trn_combined %>%

  # Note: `value_fill` is FALSE for all but some will be replaced with NA, e.g., because no full-text
  mutate(value = TRUE) %>%
  tidyr::pivot_wider(
    names_from = source, names_prefix = "has_trn_",
    values_from = value, values_fill = FALSE
  )

# Change "has_trn_SOURCE" from FALSE to NA if source *not* retrieved
trn_combined <-

  # Add info on retrieved pubmed and ft
  pubmed_ft_retrieved %>%
  select(-id, -ft_source) %>%
  janitor::remove_empty("rows") %>%
  distinct() %>%
  left_join(trn_combined, ., by = c("pmid", "doi")) %>%

  # Check that no rows added
  assertr::verify(nrow(.) == nrow(trn_combined)) %>%

  mutate(

    # TRN in secondary id and abstract are NA if no pubmed record
    has_trn_secondary_id = if_else(!has_pubmed, NA, has_trn_secondary_id),
    has_trn_abstract = if_else(!has_pubmed, NA, has_trn_abstract),

    # TRN in full-text is NA no full-text
    has_trn_ft = if_else(!has_ft, NA, has_trn_ft)
  ) %>%

  select(-has_pubmed, -has_ft)

write_rds(trn_combined, path(dir_trn, "trn-reported-wide.rds"))


# Combine all trns (intovalue and reported) -------------------------------

trn_all <-
  bind_rows(
    distinct(intovalue, trn = id, registry),
    distinct(trn_combined, trn, registry)
  ) %>%
  distinct()

write_rds(trn_all, path(dir_trn, "trn-all.rds"))
