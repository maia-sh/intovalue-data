# Get open access data from the Unpaywall API

source(here::here("R", "environment.R"))

# Filter trials with a publication and DOI and keep only unique DOIs
intovalue_dois <- intovalue %>%
  filter(has_publication, has_doi) %>%
  distinct(doi, .keep_all = TRUE)

#------------------------------------------------------------------------------#
# 1. Query Unpaywall API with journal > repository hierarchy (except Bronze)
#------------------------------------------------------------------------------#

cols <- c("doi","color","issn","journal","publisher","date")
df <- data.frame(matrix(ncol = length(cols), nrow = 0))
colnames(df) <- cols

doi_batch <- intovalue_dois[["doi"]]
print(paste0("DOI number: ", length(doi_batch)))

unpaywall_results <- unpaywallR::dois_OA_colors(doi_batch,
                                                email_api,
                                                clusters = 2,
                                                color_hierarchy = c("gold",
                                                                    "hybrid",
                                                                    "green",
                                                                    "bronze",
                                                                    "closed"))

oa_results <- tibble(doi = doi_batch,
                     color = unpaywall_results$OA_color,
                     issn = unpaywall_results$issn,
                     journal = unpaywall_results$journal,
                     publisher = unpaywall_results$publisher,
                     date = unpaywall_results$date)

df <- rbind(df, oa_results)

all_results <- left_join(intovalue, df, by = "doi")

test <- all_results %>%
  verify(nrow(.)==nrow(intovalue))

#------------------------------------------------------------------------------#
# 2. Query Unpaywall API with all OA routes > green OA hierarchy
#------------------------------------------------------------------------------#

cols_green <- c("doi","color_green_only")
df_green <- data.frame(matrix(ncol = length(cols_green), nrow = 0))
colnames(df_green) <- cols_green

print(paste0("DOI number: ", length(doi_batch)))

unpaywall_results <- unpaywallR::dois_OA_colors(doi_batch,
                                                email_api,
                                                clusters = 2,
                                                color_hierarchy = c("gold",
                                                                    "hybrid",
                                                                    "bronze",
                                                                    "green",
                                                                    "closed"))

oa_results_green <- tibble(doi = doi_batch,
                           color_green_only = unpaywall_results$OA_color)

df_green <- rbind(df_green, oa_results_green)

# Add the results to those of the first query
all_results_green <- left_join(all_results, df_green, by = "doi")

write_csv(all_results_green, paste0("data/", Sys.Date(), "_intovalue-oa.csv"))

test <- all_results_green %>%
  verify(nrow(.)==nrow(intovalue))

#NOTE: for OA plots, make sure that any given DOI is only represented once per UMC
# (unit of interest=publication)
