library(dplyr)
library(readr)
library(here)


# Get data ----------------------------------------------------------------

intovalue <- read_csv(here("data", "raw", "intovalue.csv"))
registry_crossreg <- read_rds(here("data", "processed", "registries", "registry-crossreg.rds"))
trn_reported_wide <- read_rds(here("data", "processed", "trn", "trn-reported-wide.rds"))
trn_reported_long <- read_rds(here("data", "processed", "trn", "trn-reported-long.rds"))
pubmed_ft_retrieved <- read_rds(here("data", "processed", "pubmed", "pubmed-ft-retrieved.rds"))


# Explore cross-registrations ---------------------------------------------

# There are up to 4 cross-registrations
# registry_crossreg %>%
#   group_by(id) %>%
#   summarise(n_crossreg = n()) %>%
#   group_by(n_crossreg) %>%
#   summarise(n_trials = n())

# To join with trials, could pivot wider
# registry_crossreg %>%
#   group_by(id) %>%
#   mutate(n_crossreg = row_number()) %>%
#   ungroup() %>%
#   tidyr::pivot_wider(
#     id_cols = id,
#     names_from = n_crossreg,
#     values_from = c(crossreg_registry, crossreg_trn)
#   )


# Prepare all cross-registrations -----------------------------------------

# Cross-registrations per pubmed/publication
crossreg_pub <-

  # Remove intovalue primary registrations (since already included in `trials`)
  anti_join(trn_reported_wide, intovalue, by = c("trn" = "id", "pmid")) %>%
  rename(crossreg_registry = registry, crossreg_trn = trn) %>%
  rename_with(~stringr::str_replace(., "has_trn", "is_crossreg")) %>%

  # Add in intovalue primary trn
  # Also cause duplication of some publications associated with multiple trials
  left_join(distinct(intovalue, id, pmid, doi), by = c("pmid", "doi"))

# Cross-registrations per registries
crossreg_reg <-

  # Keep only intovalue primary registrations
  # Since `registry_crossreg` also include crossreg from trials linked in iv pubs
  semi_join(registry_crossreg, intovalue, by = "id") %>%
  mutate(is_crossreg_reg = TRUE) %>%

  # Add in intovalue publication ids
  left_join(distinct(intovalue, id, pmid, doi), by = "id")

# Create lists of retrieved pubmed and ft pdf
pubmed_retrieved <-
  pubmed_ft_retrieved %>%
  filter(has_pubmed) %>%
  pull(pmid)

ft_retrieved <-
  pubmed_ft_retrieved %>%
  filter(has_ft) %>%
  pull(pmid)


cross_registrations <-
  tidylog::full_join(
    crossreg_pub, crossreg_reg,
    by = c("id", "crossreg_trn", "crossreg_registry", "pmid", "doi")
  ) %>%
  relocate(id, pmid, doi, crossreg_trn, crossreg_registry) %>%

  # Replace NAs with FALSE depending on source availability
  # Registry always available, whereas as pubmed and ft pdf depends
  mutate(is_crossreg_reg = tidyr::replace_na(is_crossreg_reg, FALSE)) %>%
  mutate(
    is_crossreg_secondary_id = if_else(
      is.na(is_crossreg_secondary_id) & pmid %in% pubmed_retrieved,
      FALSE, is_crossreg_secondary_id
    ),

    is_crossreg_abstract = if_else(
      is.na(is_crossreg_abstract) & pmid %in% pubmed_retrieved,
      FALSE, is_crossreg_abstract
    ),

    is_crossreg_ft = if_else(
      is.na(is_crossreg_ft) & pmid %in% ft_retrieved,
      FALSE, is_crossreg_ft
    )
  )

write_rds(cross_registrations, here("data", "processed", "trn", "cross-registrations.rds"))

# cross_registrations %>%
#   count(is_crossreg_secondary_id, is_crossreg_abstract, is_crossreg_ft_pdf, is_crossreg_reg)


# Prepare number of cross-registrations per intovalue trial ---------------

n_crossreg_pub <-
  trn_reported_long %>%
  # Remove intovalue primary registrations (since already included in `trials`)
  anti_join(intovalue, by = c("trn" = "id", "pmid")) %>%
  add_count(source, pmid, doi, name = "n_crossreg") %>%
  select(-trn, -registry) %>%
  distinct() %>%
  tidyr::pivot_wider(
    names_from = source,
    names_prefix = "n_crossreg_",
    values_from = n_crossreg,
    values_fill = as.integer(0) # 0 crossreg if NA
  ) %>%
  # Add in intovalue primary trn
  # Also cause duplication of some publications associated with multiple trials
  left_join(distinct(intovalue, id, pmid, doi), by = c("pmid", "doi"))


n_crossreg_reg <-
  registry_crossreg %>%

  # Change n_crossreg to sum instead of row number
  add_count(id, name = "n_crossreg_reg") %>%
  select(-crossreg_trn, -crossreg_registry) %>%

  # Add in intovalue publication ids
  left_join(distinct(intovalue, id, pmid, doi), by = "id") %>%
  distinct()


n_cross_registrations <-
  full_join(
    n_crossreg_pub, n_crossreg_reg,
    by = c("id", "pmid", "doi")
  ) %>%
  relocate(id, pmid, doi) %>%

  # Add in all intovalue trials (including those with no crossreg)
  left_join(select(intovalue, id, pmid, doi), ., by = c("id", "pmid", "doi")) %>%

  # Replace NAs with 0 depending on source availability
  # Registry always available, whereas as pubmed and ft pdf depends
  mutate(n_crossreg_reg = tidyr::replace_na(n_crossreg_reg, as.integer(0))) %>%
  mutate(
    n_crossreg_secondary_id = if_else(
      is.na(n_crossreg_secondary_id) & pmid %in% pubmed_retrieved,
      as.integer(0), n_crossreg_secondary_id
    ),

    n_crossreg_abstract = if_else(
      is.na(n_crossreg_abstract) & pmid %in% pubmed_retrieved,
      as.integer(0), n_crossreg_abstract
    ),

    n_crossreg_ft = if_else(
      is.na(n_crossreg_ft) & pmid %in% ft_retrieved,
      as.integer(0), n_crossreg_ft
    )
  )

write_rds(n_cross_registrations, here("data", "processed", "trn", "n-cross-registrations.rds"))
