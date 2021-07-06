# Prepare IntoValue with additional data:
# * cross-registration (boolean by registry)
# * drks summary results date
# * results search dates
# * open access (unpaywall + syp)

library(dplyr)
library(readr)
library(here)


# Get data ----------------------------------------------------------------

intovalue_raw <- read_csv(here("data", "raw", "intovalue.csv"))
crossreg <- read_csv(here("data", "raw", "cross-registrations.csv"))
oa_unpaywall <- read_csv(here("data", "raw", "oa-unpaywall.csv"))
oa_syp <- read_csv(here("data", "raw", "oa-syp-permissions.csv"))


# Add cross-registrations -------------------------------------------------

crossreg_registries <-
  crossreg %>%

  # Limit to cross-registrations mentioned in registry (not publication)
  filter(is_crossreg_reg) %>%

  select(-starts_with("is_crossreg_")) %>%

  # Limit to one row per cross-registration registry per trial
  # We are interested in whether a trial was cross-registered in a particular registry and not whether there were multiple registrations in that registry
  distinct(id, crossreg_registry) %>%

  # Create booleans for cross-registration in each registry
  # We want to know which registries each trial was cross-registered in
  mutate(
    crossreg_registry = tolower(crossreg_registry),
    value = TRUE
  ) %>%
  tidyr::pivot_wider(
    names_from = crossreg_registry, names_prefix = "has_crossreg_",
    values_from = value, values_fill = FALSE
  )

# Add registry info to intovalue
intovalue <-
  intovalue_raw %>%
  left_join(crossreg_registries, by = "id") %>%
  mutate(across(starts_with("has_crossreg_"), ~ tidyr::replace_na(., FALSE)))


# Add DRKS summary results date -------------------------------------------
# Note: DRKS summary results date taken from trial history and NOT pdf

intovalue <-
  intovalue %>%
  mutate(
    summary_results_date = case_when(
      id == "DRKS00003170" ~ "2013-09-24",
      id == "DRKS00000711" ~ "2013-05-17",
      id == "DRKS00004721" ~ "2013-02-11", #unsure
      id == "DRKS00003280" ~ "2014-12-10",
      id == "DRKS00004744" ~ "2016-01-14",
      id == "DRKS00005500" ~ "2018-01-31",
      id == "DRKS00005683" ~ "2017-07-07",
      id == "DRKS00013233" ~ "2017-11-06",
      id == "DRKS00011584" ~ "2019-07-09",
      id == "DRKS00000635" ~ "2019-06-07",
      id == "DRKS00000156" ~ "2017-01-17",
      id == "DRKS00003527" ~ "2018-04-25",
      id == "DRKS00006734" ~ "2017-05-20",
      id == "DRKS00006766" ~ "2019-06-06",
      id == "DRKS00007163" ~ "2019-05-17",
    ),
    summary_results_date = as.Date(summary_results_date)
  ) %>%
  arrange(summary_results_date)


# Add results search dates ------------------------------------------------
# We are interested in follow-up time, i.e., how much time between study completion and results search
# Add results search date based on 10.1016/j.jclinepi.2019.06.002 (for IntoValue1) and from NR correspondence on 2021-06-29 (for IntoValue2)
# Since date not available by trial, prepare both start and end of search period

intovalue <-

  intovalue %>%

  mutate(
    results_search_start_date = case_when(
      iv_version == 1 ~ as.Date("2017-07-01"),
      iv_version == 2 ~ as.Date("2020-07-01")
    ),
    results_search_end_date = case_when(
      iv_version == 1 ~ as.Date("2017-12-01"),
      iv_version == 2 ~ as.Date("2020-09-01")
    ),
    results_followup_start = results_search_start_date - completion_date,
    results_followup_end = results_search_end_date - completion_date,
  )


# Add open access ---------------------------------------------------------

intovalue <-
  intovalue %>%
  left_join(oa_unpaywall, by = "doi") %>%
  left_join(oa_syp, by = "doi")

# Explore intovalue -------------------------------------------------------

# Check that intovalue has same number of rows
# intovalue %>%
#   assertr::verify(nrow(.) == nrow(intovalue_raw))

# intovalue_explore <-
#   intovalue %>%
#   select(
#     id, iv_version, completion_date,
#     results_search_start_date, results_search_start_date,
#     results_followup_start, results_followup_end
#   )

# Note: Since we're using approximate search dates, plus updated completion dates from the registries, some trials (n = 15) appear to have been searched *prior* to completion (e.g., NCT00118573). TODO: decide how to handle! exclude from single plot or from entire sample
# intovalue_explore %>%
#   filter(results_followup_start < 0)

# Also, some trials appear to have less than 2 years of follow-up
# This is likely because start date changed in the registry post-hoc
# intovalue_explore %>%
#   filter(results_followup_start < 365*2)

# Some trials had more than 5 years of follow-up
# intovalue_explore %>%
#   filter(results_followup_start > 365*5)

# Save prepared intovalue -------------------------------------------------
dir <- fs::dir_create(here("data", "processed"))

write_csv(intovalue, fs::path(dir, "intovalue.csv"))
