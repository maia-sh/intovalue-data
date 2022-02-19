# Get open access data from the Unpaywall API

library(dplyr)
library(readr)
library(here)

# For the current version of the code, execute following 2 lines once
#library(devtools)
#install_github("delwen/unpaywallR")
library(unpaywallR)

dir <- fs::dir_create(here("data", "raw", "open-access"))

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

# Query Unpaywall
oa_results_raw <-
  unpaywallR::dois_OA_colors_fetch(
    intovalue_dois,
    email_api,
    clusters = 2)

# Pick OA color based on hierarchy journal > repository (except bronze)  --------

hierarchy <-
  c("gold",
    "hybrid",
    "green",
    "bronze",
    "closed")

oa_results <-
    unpaywallR::dois_OA_pick_color(
      oa_results_raw,
      hierarchy
  ) %>%
  rename(color = OA_color, publication_date_unpaywall = date)


# Pick OA color based on hierarchy all OA routes > green OA -------------

green_oa_hierarchy <-
  c("gold",
    "hybrid",
    "bronze",
    "green",
    "closed")

oa_results_green <-
  unpaywallR::dois_OA_pick_color(
    oa_results_raw,
    green_oa_hierarchy
  ) %>%
  select(doi, color_green_only = OA_color)


# Save Unpaywall data -----------------------------------------------------

oa_unpaywall <-
  full_join(oa_results_green, oa_results, by = "doi") %>%
  mutate(across(everything(), ~na_if(., "")))

write_csv(oa_unpaywall, fs::path(dir, "oa-unpaywall.csv"))


# Log query date ----------------------------------------------------------

loggit::set_logfile(here::here("queries.log"))
loggit::loggit("INFO", "Unpaywall")


# Explore Unpaywall data --------------------------------------------------

# DOIs with green but not main data
oa_unpaywall %>%
  filter(is.na(color) & !is.na(color_green_only))

# DOIs with main but not green data
oa_unpaywall %>%
  filter(!is.na(color) & is.na(color_green_only))

# DOIs with neither main nor green data
unresolved_dois <- oa_unpaywall %>%
  filter(is.na(color) & is.na(color_green_only))
print(paste("Unpaywall unresolved DOIs:", nrow(unresolved_dois)))
