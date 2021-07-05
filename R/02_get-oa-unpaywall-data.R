# Get open access data from the Unpaywall API
# NOTE: for OA plots, make sure that any given DOI is only represented once per UMC (unit of interest = publication)

library(dplyr)
library(readr)
library(here)
# devtools::install_github("NicoRiedel/unpaywallR")
library(unpaywallR)

# Prepare intovalue dois --------------------------------------------------
# Filter for trials with a publication and DOI and keep only unique DOIs

intovalue_dois <-
  read_csv(here("data", "raw", "intovalue.csv")) %>%
  filter(has_publication, !is.na(doi)) %>%
  distinct(doi) %>%
  pull(doi)

print(paste("Number of DOIs:", length(intovalue_dois)))


# Set unpaywall email -----------------------------------------------------

email_api  <-
  tryCatch({

    # Try to set  email from INI configuration file
    library(ConfigParser)
    cfg <- ConfigParser$new()
    cfg$read("config.ini")
    email_api <- cfg$get("email", NA, "login")
  },

  # Otherwise, system credential stored as "rm-email", if available
  # Else ask user and store
  error = function(err){
    ifelse(
      nrow(keyring::key_list("rm-email")) == 1,
      keyring::key_get("rm-email"),
      keyring::key_set("rm-email")
    )
  }
)

# Query Unpaywall API with journal > repository hierarchy (except  --------

repository_hierarchy <-
  c("gold",
    "hybrid",
    "green",
    "bronze",
    "closed")

oa_results <-
  unpaywallR::dois_OA_colors(
    intovalue_dois,
    email_api,
    clusters = 2,
    color_hierarchy = repository_hierarchy
  ) %>%
  rename(color = OA_color, publication_date_unpaywall = date)


# Query Unpaywall API with all OA routes > green OA hierarchy -------------

green_oa_hierarchy <-
  c("gold",
    "hybrid",
    "bronze",
    "green",
    "closed")

oa_results_green <-
  unpaywallR::dois_OA_colors(
    intovalue_dois,
    email_api,
    clusters = 2,
    color_hierarchy = green_oa_hierarchy
  ) %>%
  select(doi, color_green_only = OA_color)


# Save Unpaywall data -----------------------------------------------------

oa_unpaywall <-
  full_join(oa_results_green, oa_results, by = "doi") %>%
  mutate(across(everything(), ~na_if(., "")))

write_csv(oa_unpaywall, here("data", "raw", "oa-unpaywall.csv"))
# write_csv(oa_unpaywall, here("data", "raw", paste0(Sys.Date(), "_oa-unpaywall.csv")))


# Explore Unpaywall data --------------------------------------------------

# Some dois do not resolve in unpaywall
unresolved_dois <- setdiff(intovalue_dois, oa_unpaywall$doi)
print(paste("Unpaywall unresolved DOIs:", length(unresolved_dois)))

# Some dois have green but not main data
oa_unpaywall %>%
  filter(is.na(color) & !is.na(color_green_only))

# However no dois have main data but not green
oa_unpaywall %>%
  filter(!is.na(color) & is.na(color_green_only))

# Some dois have neither main nor green data
oa_unpaywall %>%
  filter(is.na(color) & is.na(color_green_only))
