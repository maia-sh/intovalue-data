# Prepare clinical trial dashboard data:
# * apply intovalue exclusion criteria
# * remove iv1 duplicates of iv2 trials
# * create one row per umc

library(dplyr)
library(readr)
library(here)

intovalue <- read_rds(here("data", "processed", "intovalue.rds"))

ct_intovalue_umc <-

  intovalue %>%

  filter(

    # Re-apply the IntoValue exclusion criteria
    iv_completion,
    iv_status,
    iv_interventional,
    has_german_umc_lead,

    # In case of dupes, exclude IV1 version
    !(is_dupe & iv_version == 1)
  ) %>%

  # Separate trials for individual UMCs

  mutate(lead_cities = strsplit(as.character(lead_cities), " ")) %>%
  tidyr::unnest(lead_cities)

write_csv(ct_intovalue_umc, here("data", "processed", "ct-dashboard-intovalue-umc.csv"))

ct_intovalue_all <-

  intovalue %>%

  filter(

    # Re-apply the IntoValue exclusion criteria
    iv_completion,
    iv_status,
    iv_interventional,
    has_german_umc_lead,

    # In case of dupes, exclude IV1 version
    !(is_dupe & iv_version == 1)
  )

write_csv(ct_intovalue_all, here("data", "processed", "ct-dashboard-intovalue-all.csv"))
