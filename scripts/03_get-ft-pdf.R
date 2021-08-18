library(dplyr)
library(fs)
library(readr)
library(here)
library(stringr)

# Full-text acquired via a combination of automated and manual techniques
# Full-text searched for via DOI, if available, else PMID
# Converted to XML via GROBID
# Converted XML manually added to `dir_doi_xml` and `dir_pmid_xml`

# Prepare directory paths and create if not existent
# ├── data
# │   └── raw
# │       ├── fulltext
# │       │   ├── doi
# │       │   │   ├── pdf
# │       │   │   └── xml
# │       │   └── pmid
# │       │   │   ├── pdf
# │       │   │   └── xml
dir_ft       <- here("data", "raw", "fulltext")
dir_doi      <- path(dir_ft, "doi")
dir_pmid     <- path(dir_ft, "pmid")
dir_doi_pdf  <- dir_create(path(dir_doi, "pdf"))
dir_pmid_pdf <- dir_create(path(dir_pmid, "pdf"))
dir_doi_xml  <- dir_create(path(dir_doi, "xml"))
dir_pmid_xml <- dir_create(path(dir_pmid, "xml"))

intovalue <- read_csv(here("data", "raw", "intovalue.csv"))

source(here("R", "functions", "retrieve_pdf.R"))
source(here("R", "functions", "resolve_doi.R"))
source(here("R", "functions", "correct_pdf_url.R"))

# Get pdfs from dois ------------------------------------------------------

dois <-
  intovalue %>%
  filter(!is.na(doi)) %>%
  # Alternatively, could limit to dois for pubmed records (with pmid)
  # filter(!is.na(pmid)) %>%
  distinct(doi) %>%
  arrange(doi) %>%
  pull()

# If dois already downloaded and/or converted, remove those from list to download
ft_doi_pdf <-
  dir_ls(dir_doi_pdf) %>%
  path_file() %>%
  path_ext_remove() %>%
  str_replace_all("\\+", "/")

ft_doi_xml <-
    dir_ls(dir_doi_xml) %>%
    path_file() %>%
    str_remove(".tei.xml$") %>%
    str_replace_all("\\+", "/")

# Check whether doi pdfs not converted to xml
dois_pdf_not_xml <- setdiff(ft_doi_pdf, ft_doi_xml)

# Note: GROBID could not parse some pdfs (abstracts) so expect not be in xml
dois_non_parseable <- c("10.1093/eurheartj/ehu322", "10.1186/cc14575")

if (length(dois_pdf_not_xml) > 0 && dois_pdf_not_xml != dois_pdf_not_xml){
  rlang::warn(glue::glue("Unconverted dois pdfs:{dois_pdf_not_xml}"))
}

# Check whether unneeded dois retrieved, and manually review and remove
dois_downloaded_unused <-
  union(ft_doi_pdf, ft_doi_xml) %>%
  setdiff(dois)
if (length(dois_downloaded_unused) > 0){
  rlang::warn(glue::glue("Unused dois downloaded:{dois_downloaded_unused}"))
}

# Limit to missing dois
dois <-
  union(ft_doi_pdf, ft_doi_xml) %>%
  setdiff(dois, .) %>%
  sort()

# Download remaining dois, if any
if (length(dois) > 0) {

  # Use email locally stored as "rm-email", if available
  # Else ask user and store
  email <-
    ifelse(
      nrow(keyring::key_list("rm-email")) == 1,
      keyring::key_get("rm-email"),
      keyring::key_set("rm-email")
    )

  dois %>%
    purrr::walk(retrieve_pdf,
                dir  = dir_doi_pdf,
                source = "publisher", # Or "publisher" with VPN turned on
                email = email
    )
}

# Get pdfs from pmids -----------------------------------------------------

# Limit to pmids without dois for pubmed records
pmids_no_dois <-
  intovalue %>%
  filter(!is.na(pmid) & is.na(doi)) %>%
  distinct(pmid) %>%
  pull()

# Since so few publications, manually search and add to `dir_pmid_pdf`, then convert to XML and add to `dir_pmid_xml`

# If pmids already downloaded and/or converted, remove those from list to download
ft_pmid_pdf <-
  dir_ls(dir_pmid_pdf) %>%
  path_file() %>%
  path_ext_remove() %>%
  str_replace_all("\\+", "/")

ft_pmid_xml <-
  dir_ls(dir_pmid_xml) %>%
  path_file() %>%
  str_remove(".tei.xml$") %>%
  str_replace_all("\\+", "/")

# Check whether pmid pdfs not converted to xml
pmids_pdf_not_xml <- setdiff(ft_pmid_pdf, ft_pmid_xml)
if (length(pmids_pdf_not_xml) > 0){
  rlang::warn(glue::glue("Unconverted pmids pdfs:{pmids_pdf_not_xml}"))
}

# Check whether unneeded pmids retrieved, and manually review and remove
pmids_downloaded_unused <-
  union(ft_pmid_pdf, ft_pmid_xml) %>%
  setdiff(pmids_no_dois)
if (length(pmids_downloaded_unused) > 0){
  rlang::warn(glue::glue("Unused pmids downloaded:{pmids_downloaded_unused}"))
}

# Limit to missing pmids
pmids_no_dois <-
  union(ft_pmid_pdf, ft_pmid_xml) %>%
  setdiff(pmids_no_dois, .) %>%
  sort()

