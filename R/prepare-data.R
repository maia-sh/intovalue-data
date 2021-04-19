library(dplyr)
library(fs)
library(readr)

# Get data from other repositories, assuming both repositories in same parent directory
dir_repositories <- path_norm(path_wd(".."))
dir <- path(dir_repositories, "trn-intovalue")

intovalue <- read_rds(path(dir, "data", "processed", "intovalue-pmids.rds"))
publications_all <- read_rds(path(dir, "data", "processed", "publications-ft.rds"))
linked_reg_pub <-
  read_rds(path(dir, "data", "processed", "linked-reg-pub.rds"))


# Combine publication and registry info into intovalue
intovalue %>%
  left_join(
    select(publications_all, -doi, -year, -journal),
    by = "pmid"
  ) %>%
  left_join(
    select(linked_reg_pub, id = trn, pmid, has_reg_pub_link),
    by = c("id", "pmid")
  ) %>%
  write_csv(here::here("data", "intovalue.csv"))
