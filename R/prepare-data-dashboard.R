source(here::here("R", "environment.R"))

# Get cross-registration data from the TV clinical transparency repository
dir_repositories <- path_norm(path_wd(".."))
dir <- path(dir_repositories, "tv-ct-transparency")

read_csv(path(dir, "data", "raw", "intovalue-crossreg.csv")) %>%
  write_csv(here::here("data", "intovalue-crossreg.csv"))

# Limit to EUCTR cross-registrations (in registry)
crossreg_euctr <- read_csv(here::here("data", "intovalue-crossreg.csv")) %>%
  filter(crossreg_registry == "EudraCT" & is_crossreg_reg)

# We just want to flag IV trials with a cross-registration in EudraCT, so
# in case of multiple cross-registrations in EudraCT, keep only one of them
crossreg_euctr <- distinct(crossreg_euctr, id, .keep_all = TRUE) %>%
  select(id, crossreg_trn, crossreg_registry)

# Read original IV dataset from data folder
intovalue <- read_csv(here::here("data", "intovalue.csv"))
n_trials <- nrow(intovalue)

intovalue <- left_join(intovalue, crossreg_euctr, by = "id")

test <- intovalue %>%
  verify(nrow(.)==n_trials)

# Re-apply the IntoValue exclusion criteria and de-duplicate
intovalue <- intovalue %>%

  # In case of dupes, exclude IV1 version
  mutate(is_not_iv1_dupe = if_else(!(is_dupe & iv_version == 1), TRUE, FALSE)) %>%
  # Add column to check if DOI exists
  mutate(has_doi = if_else(!is.na(doi), TRUE, FALSE)) %>%

  filter(
    iv_completion,
    iv_status,
    iv_interventional,
    has_german_umc_lead,
    is_not_iv1_dupe
  )

n_iv_trials <- nrow(intovalue)

# Add manually extracted summary results date for trials registered in DRKS
# TODO: do this more efficiently with ID-date mapping
summ_res_drks <- intovalue %>%
  filter(registry == "DRKS", has_summary_results) %>%
  select(id) %>%
  mutate(summary_results_date_drks=as.Date("", format = "%Y-%m-%d"))

summ_res_drks[summ_res_drks$id == "DRKS00003170",]$summary_results_date_drks <- as.Date("2013-09-24", format = "%Y-%m-%d")
summ_res_drks[summ_res_drks$id == "DRKS00000711",]$summary_results_date_drks <- as.Date("2013-05-17", format = "%Y-%m-%d")
summ_res_drks[summ_res_drks$id == "DRKS00004721",]$summary_results_date_drks <- as.Date("2013-02-11", format = "%Y-%m-%d") #not sure
summ_res_drks[summ_res_drks$id == "DRKS00003280",]$summary_results_date_drks <- as.Date("2014-12-10", format = "%Y-%m-%d")
summ_res_drks[summ_res_drks$id == "DRKS00004744",]$summary_results_date_drks <- as.Date("2016-01-14", format = "%Y-%m-%d")
summ_res_drks[summ_res_drks$id == "DRKS00005500",]$summary_results_date_drks <- as.Date("2018-01-31", format = "%Y-%m-%d")
summ_res_drks[summ_res_drks$id == "DRKS00005683",]$summary_results_date_drks <- as.Date("2017-07-07", format = "%Y-%m-%d")
summ_res_drks[summ_res_drks$id == "DRKS00013233",]$summary_results_date_drks <- as.Date("2017-11-06", format = "%Y-%m-%d")
summ_res_drks[summ_res_drks$id == "DRKS00011584",]$summary_results_date_drks <- as.Date("2019-07-09", format = "%Y-%m-%d")
summ_res_drks[summ_res_drks$id == "DRKS00000635",]$summary_results_date_drks <- as.Date("2019-06-07", format = "%Y-%m-%d")
summ_res_drks[summ_res_drks$id == "DRKS00000156",]$summary_results_date_drks <- as.Date("2017-01-17", format = "%Y-%m-%d")
summ_res_drks[summ_res_drks$id == "DRKS00003527",]$summary_results_date_drks <- as.Date("2018-04-25", format = "%Y-%m-%d")
summ_res_drks[summ_res_drks$id == "DRKS00006734",]$summary_results_date_drks <- as.Date("2017-05-20", format = "%Y-%m-%d")
summ_res_drks[summ_res_drks$id == "DRKS00006766",]$summary_results_date_drks <- as.Date("2019-06-06", format = "%Y-%m-%d")
summ_res_drks[summ_res_drks$id == "DRKS00007163",]$summary_results_date_drks <- as.Date("2019-05-17", format = "%Y-%m-%d")

intovalue <- intovalue %>%
  left_join(summ_res_drks, by = "id") %>%
  unite("summary_results_date_all",
        summary_results_date,
        summary_results_date_drks,
        remove = FALSE,
        na.rm = TRUE)

intovalue$summary_results_date_all <- as.Date(intovalue$summary_results_date_all,
                                              format = "%Y-%m-%d")

# summary_results_date contains automatically extracted dates (CT.gov only)
# summary_results_date_drks contains manually extracted dates (DRKS only)
# summary_results_date_all combines both for use in plots

# Separate trials for individual UMCs
intovalue <- intovalue %>%
  mutate(lead_cities = strsplit(as.character(lead_cities), " ")) %>%
  unnest(lead_cities)
