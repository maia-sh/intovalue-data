## Script to compile all current publications associated with the IV dataset and catalogue which TRNs are found in which section of the pub.

##############################################################################################

library(tidyverse)
library(readr)
library(here)
library(fs)
library(lubridate)
library(stringr)

dir_raw <- here("data", "raw")
dir_processed <- here("data", "processed")

trials <- read_csv(path(dir_processed, "trials.csv"))
cross_registrations <- read_rds(path(dir_processed, "trn", "cross-registrations.rds"))

# for our matching, we need to have cross_registration with the relevant urls attached, since not all publications have a DOI or PMID
urls <- trials %>% select(id, url)
cr_with_url <- left_join(cross_registrations, urls, by = "id")

## the above results in a warning that I'm not sure is an issue or not yet!

##############################################################################################

# starting with initial table isolating just the DOI of a pub, the PMID, the publication type, and the IV trial it is associated with from trials.csv

doi_pmid_iv <- trials %>% select(id, doi, pmid, url, publication_type) %>% rename(trns_iv = id)

# Now we will fill our publications with the TRNs that are found in each section of every paper using cross_registrations

publications <- doi_pmid_iv %>% add_column(trns_si = NA, trns_abs = NA, trns_ft = NA, trns_other = NA) %>%
  relocate(trns_iv, .after = trns_ft) %>%
  relocate(publication_type, .after = trns_other)

# We will iterate through cr_with_url, if a DOI/PMID/url in 'publications' matches with a DOI/PMID/url in cr_with_url, we will make note of
# crossreg_trn, which is the non-IV TRN found in the pub
# If the value in any of the boolean columns in cross_registrations is TRUE, then we add crossreg_trn to the corresponding column in 'publications'

update_publications <- function(doi, pmid, url, crossreg_trn, bool_col, col_name, publications) {
  matching_rows <- which(publications$doi == doi | publications$pmid == pmid | publications$url == url)
  publications[matching_rows, col_name] <- paste(publications[matching_rows, col_name], crossreg_trn, sep = ";")
  publications
}

# Iterate over rows of cr_with_url
for (i in 1:nrow(cr_with_url)) {

  # Update publications for each boolean condition and column
  publications <- update_publications(
    cr_with_url[i, "doi"],
    cr_with_url[i, "pmid"],
    cr_with_url[i, "url"],
    cr_with_url[i, "crossreg_trn"],
    cr_with_url[i, "is_crossreg_secondary_id"],
    "trns_si",
    publications
  )
  publications <- update_publications(
    cr_with_url[i, "doi"],
    cr_with_url[i, "pmid"],
    cr_with_url[i, "url"],
    cr_with_url[i, "crossreg_trn"],
    cr_with_url[i, "is_crossreg_abstract"],
    "trns_abs",
    publications
  )
  publications <- update_publications(
    cr_with_url[i, "doi"],
    cr_with_url[i, "pmid"],
    cr_with_url[i, "url"],
    cr_with_url[i, "crossreg_trn"],
    cr_with_url[i, "is_crossreg_ft"],
    "trns_ft",
    publications
  )
}
