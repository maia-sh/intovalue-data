# Prepare clinical trial dashboard data:
# * apply intovalue exclusion criteria
# * remove iv1 duplicates of iv2 trials
# * create one row per umc

library(dplyr)
library(readr)
library(here)

intovalue <- read_csv(here("data", "processed", "intovalue.csv"))

ct_intovalue <-

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

write_csv(ct_intovalue, here("data", "processed", "ct-dashboard-intovalue.csv"))
