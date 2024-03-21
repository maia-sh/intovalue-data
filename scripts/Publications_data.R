## Script to compile all publications currently associated with the IV dataset and catalog
## which TRNs are found in which section of each publication

##############################################################################################

library(tidyverse)
library(readr)
library(here)
library(fs)
library(lubridate)
library(stringr)

dir_processed <- here("data", "processed")

trials <- read_csv(path(dir_processed, "trials.csv"))
cross_registrations <- read_rds(path(dir_processed, "trn", "cross-registrations.rds"))

# 'trials' gives us more information about URL and pub_type to supplement what we know from 'cross_registration'
pubs_supplementary_info = trials %>% select(doi, url, publication_type) %>% unique()

# pubs_with_crossreg will give us information about WHERE in the publication the cross-registered TRNs can be found
pubs_from_crossreg = cross_registrations %>%
  filter(is_crossreg_secondary_id == TRUE | is_crossreg_abstract == TRUE | is_crossreg_ft == TRUE) %>% # filter out all cross-regs that aren't linked by a pub
  select(id, pmid, doi, crossreg_trn, crossreg_registry, is_crossreg_secondary_id, is_crossreg_abstract, is_crossreg_ft) %>% unique()

##############################################################################################

# Merge two above tables to get publications with url and publication type info added
pubs_with_info = merge(pubs_from_crossreg, pubs_supplementary_info, by = "doi") %>%
                 unique() %>%
                 relocate(url, .after = pmid) %>%
                 relocate(publication_type, .after = url) %>%
                 rename(primary_IV_id = id) %>%
                 relocate(primary_IV_id, .after = publication_type)

# Once merged, can start putting TRNs in semicolon separated list in each column; initialize new columns
pubs_with_info$trns_si <- NA
pubs_with_info$trns_abs <- NA
pubs_with_info$trns_ft <- NA
pubs_with_info$trns_other <- NA


for (i in 1:nrow(pubs_with_info)) {

  # If the crossreg_trn column in a row isn't blank AND is_crossreg_secondary is TRUE, then add the crossreg_trn in semicolon-separated
  # list to trns_si
  if (!is.na(pubs_with_info$crossreg_trn[i]) & pubs_with_info$is_crossreg_secondary_id[i]) {
    pubs_with_info$trns_si[i] <- paste(pubs_with_info$crossreg_trn[i], pubs_with_info$trns_si[i], sep = ";")
  }

  # Same logic as above but with is_crossreg_abstract and trns_abs
  if (!is.na(pubs_with_info$crossreg_trn[i]) & pubs_with_info$is_crossreg_abstract[i]) {
    pubs_with_info$trns_abs[i] <- paste(pubs_with_info$crossreg_trn[i], pubs_with_info$trns_abs[i], sep = ";")
  }

  # is_crossreg_ft and trns_ft
  if (!is.na(pubs_with_info$crossreg_trn[i]) & pubs_with_info$is_crossreg_ft[i]) {
    pubs_with_info$trns_ft[i] <- paste(pubs_with_info$crossreg_trn[i], pubs_with_info$trns_ft[i], sep = ";")
  }
}

# Drop unnecessary columns
publications_clean = pubs_with_info %>%
                     select(-is_crossreg_secondary_id, -is_crossreg_abstract, -is_crossreg_ft)

# Now we have multiple rows per publication, so lets collapse the table into one row per publication while conserving
# all the information

publications_final <- publications_clean %>%
  group_by(pmid, doi, url, primary_IV_id) %>%
  summarize(trns_si = paste(trns_si, collapse = ";"),
            trns_abs = paste(trns_abs, collapse = ";"),
            trns_ft = paste(trns_ft, collapse = ";"),
            url = paste(url, collapse = ";"),
  )

saveRDS(publications_final, "publications_final.rds")
