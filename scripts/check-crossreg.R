library(readr)
library(dplyr)
library(here)
library(stringr)
library(glue)

intovalue <- read_rds(here("data", "processed", "trials.rds"))
crossreg_raw <- read_rds(here("data", "processed", "trn", "cross-registrations.rds"))



# Prepare functions and data for cross-registrations ----------------------

# Add column to crossreg about whether trial included in trackvalue
trackvalue <-
  intovalue %>%
  # Limit the trials to IV 2 from Charité
  filter(str_detect(lead_cities, "Berlin") & iv_version == 2) %>%

  # Reapply IV inclusion criteria
  filter(iv_completion, iv_status, iv_interventional) %>%

  pull(id)

# Solution from https://stackoverflow.com/a/70033706/6149975
# Collapse rows across group and remove duplicates and NAs
paste_rows <- function(x) {
  unique_x <- unique(x[!is.na(x)])
  if (length(unique_x) == 0) {
    unique_x <- NA
  }

  str_c(unique_x, collapse = "; ") %>%
    str_split("; ") %>%
    unlist() %>%
    unique() %>%
    str_c(collapse = "; ")

}


# Prepare cross-registrations ---------------------------------------------

crossreg <-
  crossreg_raw %>%

  # Correct PACTR trns (TODO: fix regex in `ctregistries`)
  mutate(crossreg_trn = case_when(
    crossreg_trn == "PACTR200901000091175" ~ "PACTR2009010000911750",
    crossreg_trn == "PACTR201002000142934" ~ "PACTR2010020001429343",
    TRUE ~ crossreg_trn
  )) %>% #filter(crossreg_registry == "PACTR") %>% pull(crossreg_trn)

  # Prepare URLs
  mutate(
    id_url = if_else(
      str_detect(id, "^NCT"),
      glue("https://clinicaltrials.gov/ct2/show/{id}"),
      glue("https://www.drks.de/drks_web/navigate.do?navigationId=trial.HTML&TRIAL_ID={id}")
    ),

    crossreg_trn_url = case_when(
      crossreg_registry == "ClinicalTrials.gov" ~
        glue("https://clinicaltrials.gov/ct2/show/{crossreg_trn}"),
      crossreg_registry == "DRKS" ~
        glue("https://www.drks.de/drks_web/navigate.do?navigationId=trial.HTML&TRIAL_ID={crossreg_trn}"),
      crossreg_registry == "EudraCT" ~
        glue("https://www.clinicaltrialsregister.eu/ctr-search/search?query=eudract_number:{crossreg_trn}"),
      crossreg_registry == "ISRCTN" ~
        glue("https://doi.org/10.1186/{crossreg_trn}"),

      crossreg_registry == "ANZCTR" ~
        glue("https://anzctr.org.au/{crossreg_trn}.aspx"),
      crossreg_registry == "NTR" ~
        glue('https://www.trialregister.nl/trials?search="{crossreg_trn}"'),

      # Hard code PACTR URLs
      crossreg_registry == "PACTR" & crossreg_trn == "PACTR2009010000911750" ~
        "https://pactr.samrc.ac.za/TrialDisplay.aspx?TrialID=91",
      crossreg_registry == "PACTR" & crossreg_trn == "PACTR2010020001429343" ~
        "https://pactr.samrc.ac.za/TrialDisplay.aspx?TrialID=142",
      crossreg_registry == "PACTR" & crossreg_trn == "PACTR201009000252144" ~
        "https://pactr.samrc.ac.za/TrialDisplay.aspx?TrialID=252"
    ),

    pmid = if_else(
      !is.na(pmid),
      glue('=HYPERLINK("https://pubmed.ncbi.nlm.nih.gov/{pmid}","{pmid}")'),
      NA_character_
    ),
    doi = if_else(
      !is.na(doi),
      glue('=HYPERLINK("https://doi.org/{doi}","{doi}")'),
      NA_character_
    ),
    id_url = glue('=HYPERLINK("{id_url}","{id}")'),
    crossreg_trn_url = glue('=HYPERLINK("{crossreg_trn_url}","{crossreg_trn}")')
  ) %>%

  # Remove exact duplicates from intovalue 1 and 2
  distinct() %>%

  # Some (n = 9) trials have different publications for intovalue 1 and 2, so collapse
  group_by(id, crossreg_trn) %>%
  summarise(across(everything(), paste_rows)) %>%
  ungroup() %>%

  # Add count of crossreg per primary id
  group_by(id) %>%
  mutate(n_crossreg = row_number()) %>%
  ungroup() %>%

  mutate(trackvalue = if_else(id %in% trackvalue, TRUE, FALSE), .after = "doi") %>%
  select(
    id,
    crossreg_trn,
    trackvalue,
    id_url,
    pmid,
    doi,
    n_crossreg,
    crossreg_trn_url,
    crossreg_registry,
    is_crossreg_secondary_id,
    is_crossreg_abstract,
    is_crossreg_ft,
    is_crossreg_reg
  ) %>%
  arrange(desc(trackvalue)) %>%
  mutate(
    resolves = "", #TRUE/FALSE
    matches = "", #TRUE/FALSE (same trial, same participants)
    non_match_source = "", #iv_registration, publication, crossreg_registration, other
    non_match_rationale = "", #free text: explanation why categorized as non-match, may be quote
    has_summary_results = "", #TRUE/FALSE
    start_date = "", #DATE
    registration_date = "", #DATE
    completion_date = "", #DATE
    more_crossreg = "", #free text; semicolon-separated list of any additional crossreg mentioned
    comments= "", #free text
  )

write_csv(crossreg, here("data", "processed", "trn", "intovalue-cross-registrations.csv"))

# Explore cross-registrations ---------------------------------------------

nrow(crossreg)
n_distinct(crossreg$id)

# How many trials have how many cross-registrations?
crossreg %>%
  count(id, name = "n_crossreg_per_trial") %>%
  count(n_crossreg_per_trial, name = "n_trials")

# Which registries?
crossreg %>%
  count(crossreg_registry) %>%
  arrange(desc(n))

# In our automated step, we found `r nrow(crossreg)` potential cross-registrations across `r n_distinct(crossreg$id)` trials. We excluded # cross-registrations from associated but separate trials, as described in the primary registration (n = #) and the publication (n = #). We excluded # cross-registrations after comparing the primary and EUCTR registrations as implausible matches. By searching EUCTR for all primary registration TRNs included in our sample, we found 1 EUCTR registration linked to a DRKS TRN (2011-004463-69/DRKS00003246) and 2 EUCTR registration linked to a ClinicalTrials.gov TRNs (2014-004731-39/NCT02421172 and 2010-024264-18/NCT01471782); of these, only 1 (2014-004731-39/NCT02421172) was a trial not previously flagged in out automated search of the primary registry. After the manual checks, we found # EUCTR cross-registrations across # trials.

# Search for tv ctgov and drks trns in euctr
# Manually add any additional cross-registrations to "euctr-crossreg.csv"
euctr_query <- "https://www.clinicaltrialsregister.eu/ctr-search/search?query="

# drks_euctr_query <-
#   intovalue %>%
#   filter(registry == "DRKS") %>%
#   pull(id) %>%
#   str_remove("^DRKS") %>%
#   str_c(collapse = "+OR+") %>%
#   str_c(euctr_query, "DRKS+AND+(", ., ")")
#
# drks_ids_query <-
#   intovalue %>%
#   filter(registry == "DRKS") %>%
#   pull(id) %>%
#   str_remove("^DRKS")
#
# drks_euctr_query_chunks <- split(drks_ids_query, ceiling(seq_along(drks_ids_query)/100))
#
# drks_euctr_query <-
#   str_c(collapse = "+OR+") %>%
#   str_c(euctr_query, "DRKS+AND+(", ., ")")
#
# split(drks_ids_query, ceiling(seq_along(drks_ids_query)/100)) %>%
#   str_c(collapse = "+OR+") %>%
#   str_c(euctr_query, "DRKS+AND+(", ., ")")
#
# ctgov_euctr_query <-
#   intovalue %>%
#   filter(registry == "ClinicalTrials.gov") %>%
#   pull(id) %>%
#   str_c(collapse = "+OR+") %>%
#   str_c(euctr_query, .)
