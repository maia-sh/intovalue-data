library(dplyr)
library(fs)
library(here)

source(here("R", "functions", "download_pubmed.R"))

dir <- dir_create(here("data", "raw", "pubmed"))
intovalue <- readr::read_csv(here("data", "raw", "intovalue.csv"))

pmids <-
  intovalue %>%
  tidyr::drop_na(pmid) %>%
  distinct(pmid) %>%
  pull()

# If pmids already downloaded, remove those from list to download
if (dir_exists(dir)){

  pmids_downloaded <-
    dir_ls(dir) %>%
    path_file() %>%
    path_ext_remove() %>%
    as.numeric()

  # Check whether pmids downloaded which aren't needed and manually review and remove
  pmids_downloaded_unused <- setdiff(pmids_downloaded, pmids)
  if (length(pmids_downloaded_unused) > 0) {
    rlang::warn(glue::glue("Unused pmid downloaded: {pmids_downloaded_unused}"))
  }

  pmids <- setdiff(pmids, pmids_downloaded)
}

# Download remaining pmids, if any
if (length(pmids) > 0) {

  # Use pubmed api key locally stored as "ncbi-pubmed", if available
  # Else ask user and store
  pubmed_api_key <-
    ifelse(
      nrow(keyring::key_list("ncbi-pubmed")) == 1,
      keyring::key_get("ncbi-pubmed"),
      keyring::key_set("ncbi-pubmed")
    )

  pmids %>%
    purrr::walk(download_pubmed,
                dir = dir,
                api_key = pubmed_api_key
    )

  # Log query date
  loggit::set_logfile(here::here("queries.log"))
  loggit::loggit("INFO", "PubMed")
}
