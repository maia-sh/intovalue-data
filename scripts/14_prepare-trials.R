library(dplyr)
library(tidyr)
library(readr)
library(here)
library(fs)
library(lubridate)

source(here("scripts", "functions", "duration_days.R"))

# Get data ----------------------------------------------------------------

dir_raw <- here("data", "raw")
dir_processed <- here("data", "processed")


intovalue <- read_csv(path(dir_raw, "intovalue.csv"))
registry_studies <- read_rds(path(dir_processed, "registries", "registry-studies.rds"))
registry_references <- read_rds(path(dir_processed, "registries", "registry-references.rds"))
cross_registrations <- read_rds(path(dir_processed, "trn", "cross-registrations.rds"))
n_cross_registrations <- read_rds(path(dir_processed, "trn", "n-cross-registrations.rds"))
trn_reported_wide <- read_rds(path(dir_processed, "trn", "trn-reported-wide.rds"))
pubmed_ft_retrieved <- read_rds(path(dir_processed, "pubmed", "pubmed-ft-retrieved.rds"))
pubmed_main <- read_rds(path(dir_processed, "pubmed", "pubmed-main.rds"))
oa_unpaywall <- read_csv(path(dir_raw, "open-access", "oa-unpaywall.csv"))
oa_syp <- read_csv(path(dir_raw, "open-access", "oa-syp-permissions.csv"))


# Prepare intovalue columns -----------------------------------------------

# Exclude "days_DATE_to_publication" columns since will recalculate from registries
iv_cols_to_keep <-
  setdiff(colnames(intovalue), colnames(registry_studies)) %>%
  setdiff(stringr::str_subset(., "^days_")) %>%
  c("id", .)

trials <-

  intovalue %>%

  # Select intovalue columns to keep
  select(all_of(iv_cols_to_keep))


# Edit publication types --------------------------------------------------
# Edit publication type for trials with non-journal article dois

dissertations <- c(
  "10.15496/publikation-9460",
  "10.25593/978-3-96147-095-2"
)

abstracts <- c(
  "10.1007/s00066-014-0677-2",
  "10.1007/s00106-016-0243-6",
  "10.1016/j.bbmt.2011.12.041",
  "10.1016/j.nmd.2013.06.580",
  "10.1016/s0016-5085(15)31431-1",
  "10.1016/s0167-8140(15)30355-8",
  "10.1016/s0168-8278(11)60030-5",
  "10.1016/s0959-8049(11)70845-0",
  "10.1016/s0959-8049(16)31435-6",
  "10.1016/s1359-6349(09)71339-4",
  "10.1093/annonc/mdx369.044",
  "10.1093/neuonc/nov229.01",
  "10.1093/schbul/sby016.317",
  "10.13140/rg.2.2.19672.85765", #poster
  "10.3205/11dkvf049"
)

# Additional edge cases currently kept as journal articles
# "10.17116/terarkh201587547-52" (Russian article with English abstract)

# We consider letters as journal articles
# letters <- c(
#   "10.1038/mp.2012.196",
#   "10.1016/j.jacc.2018.03.448",
#   "10.1016/j.ijcard.2014.03.178",
#   "10.1007/s00415-011-5906-3",
#   "10.1016/j.jacc.2013.08.691", #correspondence
#   "10.1038/s41430-018-0371-z" #brief report
# )

trials <-
  trials %>%

  # Remove publication boolean and use type instead
  select(-has_publication) %>%

  mutate(
    publication_type = case_when(

      # IV1 identified some publications as abstracts
      identification_step == "Abstract only" ~ "abstract",

      # Correct some publication types based on manual inspection
      doi %in% abstracts ~ "abstract",
      doi %in% dissertations ~ "dissertation",
      TRUE ~ publication_type
    )
  )

# Add registry data -------------------------------------------------------

trials <-
  trials %>%

  # Add updated registry data
  left_join(registry_studies, by = "id") %>%

  # Add DRKS summary results dates (manually taken from trial history and NOT pdf)
  mutate(
    summary_results_date = case_when(
      id == "DRKS00003170" ~ as.Date("2013-09-24"),
      id == "DRKS00000711" ~ as.Date("2013-05-17"),
      id == "DRKS00004721" ~ as.Date("2013-02-11"), #unsure
      id == "DRKS00003280" ~ as.Date("2014-12-10"),
      id == "DRKS00004744" ~ as.Date("2016-01-14"),
      id == "DRKS00005500" ~ as.Date("2018-01-31"),
      id == "DRKS00005683" ~ as.Date("2017-07-07"),
      id == "DRKS00013233" ~ as.Date("2017-11-06"),
      id == "DRKS00011584" ~ as.Date("2019-07-09"),
      id == "DRKS00000635" ~ as.Date("2019-06-07"),
      id == "DRKS00000156" ~ as.Date("2017-01-17"),
      id == "DRKS00003527" ~ as.Date("2018-04-25"),
      id == "DRKS00006734" ~ as.Date("2017-05-20"),
      id == "DRKS00006766" ~ as.Date("2019-06-06"),
      id == "DRKS00007163" ~ as.Date("2019-05-17"),
      TRUE ~ summary_results_date
    )
  ) %>%

  # Updates days_to calculations to reflect updated dates
  mutate(

    days_cd_to_publication = duration_days(completion_date, publication_date),
    days_pcd_to_publication = duration_days(primary_completion_date, publication_date),
    days_reg_to_publication = duration_days(registration_date, publication_date),

    days_cd_to_summary = duration_days(completion_date, summary_results_date),
    days_pcd_to_summary = duration_days(primary_completion_date, summary_results_date)
  )


# Add timely summary results and publication ------------------------------

trials <-

  trials %>%

  # Add results search dates
  # We are interested in follow-up time, i.e., time between study completion and results search
  # IntoValue recorded the start and end of the manual search period and not the search dates for individual trials
  # Search dates are taken from the dataset README (https://doi.org/10.5281/zenodo.5141342)
  # For parity with IntoValue, we calculate follow-up time based on the end of the search periods (`results_search_end_date`)
  # IV1: https://github.com/quest-bih/IntoValue2/blob/master/code/3_results_analysis/Paper_Plots_Tables_IntoValue2.R#L209
  # IV2: https://github.com/quest-bih/IntoValue2/blob/master/code/3_results_analysis/Paper_Plots_Tables_IntoValue2.R#L180
  mutate(
    results_search_start_date = case_when(
      iv_version == 1 ~ as.Date("2017-07-01"),
      iv_version == 2 ~ as.Date("2020-07-01")
    ),
    results_search_end_date = case_when(
      iv_version == 1 ~ as.Date("2017-12-01"),
      iv_version == 2 ~ as.Date("2020-09-01")
    ),

    # Using end of search period
    results_followup = duration_days(completion_date, results_search_end_date),

    has_followup_2y = results_followup >= 365*2,
    has_followup_5y = results_followup >= 365*5
  ) %>%

  # Create booleans for timeliness within 1 year (summary results) and 2 years (summary results and publication)
  # NA for trials without summary results/publication
  mutate(
    is_summary_results_1y = days_cd_to_summary < 365*1,
    is_summary_results_2y = days_cd_to_summary < 365*2,
    is_summary_results_5y = days_cd_to_summary < 365*5,
    is_publication_2y = days_cd_to_publication < 365*2,
    is_publication_5y = days_cd_to_publication < 365*5
  )

# Add pubmed and full-text ------------------------------------------------

trials <-
  trials %>%

  # Add info about pubmed and ft (pdf) retrieval
  left_join(pubmed_ft_retrieved, by = c("id", "doi", "pmid")) %>%

  # Add pubmed metadata
  left_join(
    select(
      pubmed_main, pmid,
      pub_title = title, journal_pubmed = journal,
      ppub_date = ppub, epub_date = epub
    ),
    by = "pmid"
  )


# Add intovalue trns ------------------------------------------------------

trials <-
  trials %>%

  # Add info about whether intovalue trn in secondary id, abstract, ft pdf
  left_join(trn_reported_wide, by = c("id" = "trn", "registry", "doi", "pmid")) %>%

  # Check that same number of rows as intovalue
  # assertr::verify(nrow(.) == nrow(intovalue)) %>%

  # Trials without trn reported anywhere are not in `trn_reported` and have NA for all `has_trn`
  # However, `has_trn` should be FALSE if source retrieved
  # Also, rename to clarify that iv_trn (not any trn)
  mutate(
    has_iv_trn_abstract =
      if_else(has_pubmed & is.na(has_trn_abstract), FALSE, has_trn_abstract),
    has_iv_trn_secondary_id =
      if_else(has_pubmed & is.na(has_trn_secondary_id), FALSE, has_trn_secondary_id),
    has_iv_trn_ft =
      if_else(has_ft & is.na(has_trn_ft), FALSE, has_trn_ft)
  ) %>%
  select(-starts_with("has_trn_")) %>%

  # If trial has pubmed, trn in abstract/si must not be NA
  pointblank::col_vals_not_null(
    vars(has_iv_trn_secondary_id, has_iv_trn_secondary_id),
    preconditions = ~ . %>% filter(has_pubmed)
  ) %>%

  # If trial has ft pdf, trn in ft pds must not be NA
  pointblank::col_vals_not_null(
    vars(has_iv_trn_ft),
    preconditions = ~ . %>% filter(has_pubmed & has_ft)
  )


# Add additional trns (cross-registrations) -------------------------------

# Get info about registries mentioned in registrations
crossreg_registries <-
  cross_registrations %>%

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


trials <-
  trials %>%

  # Add number of cross-registrations in registry, secondary id, abstract, ft pdf
  left_join(distinct(n_cross_registrations), by = c("id", "doi", "pmid")) %>%

  # Add booleans for registries mentioned in registrations
  left_join(crossreg_registries, by = "id") %>%
  mutate(across(starts_with("has_crossreg_"), ~ tidyr::replace_na(., FALSE)))


# Add registry references links -------------------------------------------
# any, any with doi or pmid, intovalue

# Number of references in registration
references_any <-
  registry_references %>%
  count(id, name = "n_reg_pub_any")

# Limit references to unique with doi or pmid
registry_references <-
  registry_references %>%
  filter(!(is.na(doi) & is.na(pmid))) %>%
  distinct()

# Number of references with doi or pmid in registration
references_doi_or_pmid <-
  registry_references %>%
  count(id, name = "n_reg_pub_doi_or_pmid")

pmid_references <-
  registry_references %>%
  select(-doi, -reference_type) %>%
  drop_na(pmid) %>%
  mutate(pmid_link = TRUE) %>%
  distinct()

doi_references <-
  registry_references %>%
  select(-pmid, -reference_type) %>%
  drop_na(doi) %>%
  mutate(doi_link = TRUE) %>%
  distinct()

trials <-
  trials %>%

  # Join in registry references (any, and pmid or doi)
  left_join(references_any, by = "id") %>%
  left_join(references_doi_or_pmid, by = "id") %>%
  mutate(
    n_reg_pub_any = replace_na(n_reg_pub_any, as.integer(0)),
    n_reg_pub_doi_or_pmid = replace_na(n_reg_pub_doi_or_pmid, as.integer(0))
  ) %>%
  # Join in intovalue registry references by pmid
  left_join(pmid_references, by = c("id", "pmid")) %>%
  left_join(doi_references, by = c("id", "doi")) %>%

  # Check that same number of rows as trials
  # Rows would be added if kept `reference_type`, since same pub may have multiple `reference_type`
  assertr::verify(nrow(.) == nrow(trials)) %>%

  mutate(
    reference_derived = coalesce(reference_derived.x, reference_derived.y),
    .keep = "unused"
  ) %>%

  mutate(

    # Add FALSE if trials has doi/pmid and no link found
    doi_link = case_when(
      !is.na(doi_link) ~ doi_link,
      !is.na(doi) ~ FALSE,
      TRUE ~ NA
    ),

    pmid_link = case_when(
      !is.na(pmid_link) ~ pmid_link,
      !is.na(pmid) ~ FALSE,
      TRUE ~ NA
    ),

    # Trial has a registry-publication link if either doi/pmid link
    has_reg_pub_link = case_when(
      doi_link | pmid_link ~ TRUE,
      !doi_link | !pmid_link ~ FALSE,
      is.na(doi_link) & is.na(pmid_link) ~ NA
    )
  )

# Add open access ---------------------------------------------------------

trials <-
  trials %>%

  # Join in unpaywall and share your paper data
  left_join(oa_unpaywall, by = "doi") %>%
  left_join(oa_syp, by = "doi") %>%

  # Rename journal (since also pubmed journal)
  rename(journal_unpaywall = journal) %>%

  # Create booleans for whether publication is OA and, if not, whether can be archived
  # `is_oa` is TRUE for publications that are either gold, green, or hybrid; FALSE if closed or bronze; NA if no unpaywall data
  # `is_archivable` TRUE if EITHER accepted or published version may be archived according to SYP, regardless of unpaywall status;
  #                 FALSE if NEITHER accepted nor published can be archived regardless of unpaywall status
  # `is_closed_archivable` is NA if `is_oa` or no unpaywall OA data, TRUE if EITHER accepted or published version may be archived according to SYP AND publication is closed according to unpaywall, FALSE if NEITHER accepted or published version may be archived according to SYP AND publication is closed according to unpaywall
  mutate(
    is_oa = color == "gold" | color == "hybrid" | color == "green",
    is_archivable = case_when(
      permission_accepted | permission_published ~ TRUE,
      !(permission_accepted | permission_published) ~ FALSE,
      TRUE ~ NA
    ),
    is_closed_archivable = if_else(is_oa, NA, is_archivable, missing = NA)
  )


# Reapply IntoValue inclusion criteria ------------------------------------

trials <-
  trials %>%
  mutate(

    # IntoValue includes trials completed 2009-2017
    iv_completion = if_else(
      completion_year > 2008 & completion_year < 2018,
      TRUE, FALSE, missing = FALSE
    ),

    # IntoValue includes all drks and some ctgov recruitment statuses
    iv_status = if_else(
      registry == "DRKS" |
        (registry == "ClinicalTrials.gov" & recruitment_status %in% c("Completed" , "Terminated" , "Suspended", "Unknown status")),
      TRUE, FALSE
    ),

    # IntoValue includes only interventional studies
    iv_interventional = if_else(study_type == "Interventional", TRUE, FALSE)
  ) %>%

  # Unify date variable type
  mutate(across(c(ends_with("_date"), -ppub_date), as.Date))


# Reorganize columns ------------------------------------------------------

trials <-
  trials %>%
  relocate(
    "id",
    "registry",
    "is_resolved",

    # IntoValue info
    "iv_version",
    "identification_step",
    "is_dupe",
    "iv_completion",
    "iv_status",
    "iv_interventional",

    # Registry info (including derived)
    "has_german_umc_lead",
    "lead_cities",
    "facility_cities",
    "title",
    "center_size",
    "main_sponsor",
    "study_type",
    "intervention_type",
    "phase",
    "enrollment",
    "recruitment_status",
    "masking",
    "allocation",
    "is_randomized",
    "is_multicentric",
    "is_prospective",
    "has_summary_results",

    # Dates
    "registration_date",
    "start_date",
    "completion_date",
    "completion_year",
    "primary_completion_date",
    "primary_completion_year",
    "summary_results_date",

    "days_cd_to_summary",
    "days_pcd_to_summary",
    "days_reg_to_start",
    "days_reg_to_cd",
    "days_reg_to_pcd",
    "days_cd_to_publication",
    "days_pcd_to_publication",
    "days_reg_to_publication",
    "results_search_start_date",
    "results_search_end_date",

    # Publication info
    "doi",
    "pmid",
    "url",
    "publication_date",
    "publication_type",
    "has_pubmed",
    "has_ft",
    "ft_source",
    "pub_title",
    "journal_pubmed",
    "ppub_date",
    "epub_date",

    # TRN/registration-publication link
    "has_iv_trn_abstract",
    "has_iv_trn_secondary_id",
    "has_iv_trn_ft",
    "has_reg_pub_link",
    "pmid_link",
    "doi_link",
    "reference_derived",
    starts_with("has_crossreg_"),
    "n_crossreg_secondary_id",
    "n_crossreg_abstract",
    "n_crossreg_ft",
    "n_crossreg_reg",
    "n_reg_pub_any",
    "n_reg_pub_doi_or_pmid",

    # Timely results
    "results_followup",
    "has_followup_2y",
    "has_followup_5y",
    "is_summary_results_1y",
    "is_summary_results_2y",
    "is_summary_results_5y",
    "is_publication_2y",
    "is_publication_5y",

    # OA
    "color_green_only",
    "color",
    "issn",
    "journal_unpaywall",
    "publisher",
    "publication_date_unpaywall",
    "syp_response",
    "can_archive",
    "archiving_locations",
    "inst_repository",
    "versions",
    "submitted_version",
    "accepted_version",
    "published_version",
    "licenses_required",
    "permission_issuer",
    "embargo",
    "date_embargo_elapsed",
    "is_embargo_elapsed",
    "permission_accepted",
    "permission_published",
    "is_oa",
    "is_archivable",
    "is_closed_archivable"
  )



# Check that all intovalue columns in trials (except `has_publication`)
if (!rlang::is_empty(setdiff(colnames(intovalue), c(colnames(trials), "has_publication")))) {
  rlang::warn("There are intovalue columns missing from trials!")
}
# Check that same number of rows as intovalue
trials %>%
  assertr::verify(nrow(.) == nrow(intovalue))

write_rds(trials, here("data", "processed", "trials.rds"))
write_csv(trials, here("data", "processed", "trials.csv"))
