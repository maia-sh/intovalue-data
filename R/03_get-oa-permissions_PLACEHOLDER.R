# Create placeholder oa syp datafile
# TODO: rerun python syp script on full unique doi list (i.e., before applying exclusion criteria)

library(dplyr)

intovalue_oa_syp <-
  readr::read_csv(here::here("data", "2021-06-23_intovalue-oa-permissions.csv"))

oa_syp <-
  intovalue_oa_syp %>%
  select(
    doi,
    can_archive,
    archiving_locations,
    inst_repository,
    versions,
    submitted_version,
    accepted_version,
    published_version,
    licenses_required,
    permission_issuer,
    embargo,
    date_embargo_elapsed,
    is_embargo_elapsed,
    permission_accepted,
    permission_published
  ) %>%
  distinct()

readr::write_csv(oa_syp, here::here("data", "raw", "oa-syp-permissions.csv"))
