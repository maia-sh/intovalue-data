# This script offers an alternate approach to `get-crossreg-registries.R`
# for extracting ids from euctr protocol and results.
# `get-crossreg-registries.R` relies on for loops, which are slow
# This script uses tidyverse and a vectorized approach and is faster.
#
# It also contains a few changes in design choices:
# namely, instead of removing all text once an id is recognized (as the original does),
# this script removes only the id from the text, as the text may have additional information.
#
# This script first processes the more complex "other ids" field,
# and then combines this with the simpler isrctn and ctgov fields
#
# Note: It's done for protocol only and would need to be extended for results.
# Note: Output hasn't been compared to final original output due to runtime issues.

# Function to collapse rows across group and remove duplicates and NAs
paste_rows <- function(x, collapse = ", ") {
  unique_x <- unique(x[!is.na(x)])
  if (length(unique_x) == 0) {
    unique_x <- NA
  }

  paste(unique_x, collapse = collapse)
}

# other-ids ---------------------------------------------------------------
eu_protocol_other_ids <-

  eu_protocol_dump |>
  distinct(eudract_number, other_ids = other_identifiers) |>

  # Separate into row per id
  separate_longer_delim(other_ids, " \n \n \n ") |>

  drop_na(other_ids) |>

  # Some ids have new line character which cause issues, so clean up
  # "2016-003489-25-IT", "2014-004818-28-IT"
  mutate(
    other_ids = str_replace(other_ids, "Number: \n", "Number: "),
    other_ids = str_replace(other_ids, "\n", " ")
  ) |>

  # Separate name and number
  separate_wider_regex(other_ids, c("Name: ", other_name = ".*(?= Number: )", " Number: ", other_number = ".*")) |>

  # Remove extra spaces
  mutate(across(c(other_name, other_number), str_squish)) |>

  distinct() |>

  # NO EXTRA CLEANING
  # # Some data should be NA (e.g., punctuation, NA, ND = no data, TBC = to be confirmed)
  # mutate(across(
  #   c(other_name, other_number),
  #   ~if_else(str_detect(., "(?i)^(([-./0]+)|(n.?[ad].?)|(not applic?able)|(not available)|\\(no other descriptors\\)|(tbc)|(pending)|(to be completed soon)|(volgt))$"), NA_character_, ., missing = .)
  # )) |>
  #
  # # Remove rows where both name and number NA
  # filter(!(is.na(other_name) & is.na(other_number)))

  # other-name --------------------------------------------------------------

  # Extract trn, if available
  ctregistries::mutate_trn_registry(by = other_name) |>

  mutate(

    # If trn found, remove from unclean
    other_names_protocol_unclean =
      if_else(!is.na(trn), str_remove_all(other_name, trn) |> na_if(""), other_name),

    # Clean any trns found
    other_name = purrr::map_chr(trn, ctregistries::clean_trn),
  ) |>
  select(-registry, -trn) |>

  # other-number ------------------------------------------------------------

  # Extract trn, if available
  ctregistries::mutate_trn_registry(by = other_number) |>

  mutate(

    # If trn found, remove from unclean
    other_numbers_protocol_unclean =
      if_else(!is.na(trn), str_remove_all(other_number, trn) |> na_if(""), other_number),

    # Clean any trns found
    other_number = purrr::map_chr(trn, ctregistries::clean_trn),
  ) |>
  select(-registry, -trn) |>

  distinct() |>

  # If name/number same, replace one with NA
  mutate(other_name = if_else(!is.na(other_name) & !is.na(other_number) & str_equal(other_name, other_number), NA_character_, other_name, NA_character_ )) |>

  # Unite other ids from name/number
  unite(other_ids, other_name, other_number, sep = ";", na.rm = TRUE) |>
  mutate(other_ids = na_if(other_ids, "")) %>%

  # Remove trn from unclean across trial
  # https://stackoverflow.com/questions/78756190/remove-string-from-column-across-group-of-rows-in
  mutate(
    across(
      matches("other_n\\w+_protocol_unclean"),
      ~str_remove_all(., paste(other_ids, collapse = '|'))
    ),
    .by = eudract_number
  ) |>

  # We will use "|" and hard brackets to combine name/number
  # So replace any occurrences
    mutate(across(
      matches("other_n\\w+_protocol_unclean"),
      ~str_replace_all(., "\\|", ";") |>
        str_replace_all("\\[", "\\(") |>
        str_replace_all("\\]", "\\)") |>
        str_trim()
    )) |>

  # Remove duplicate inform in unclean name/number, within and across rows (i.e., country registrations)

  # If unclean name/number same, replace one with NA
  mutate(other_numbers_protocol_unclean = if_else(!is.na(other_numbers_protocol_unclean) & !is.na(other_names_protocol_unclean) & str_equal(other_numbers_protocol_unclean, other_names_protocol_unclean), NA_character_, other_numbers_protocol_unclean, NA_character_ )) |>

  # Remove duplicate name/number across rows (i.e., country registrations)
  mutate(row_id = row_number()) |>
  pivot_longer(ends_with("protocol_unclean"), names_to = "other_type", values_to = "other_value") |>
  drop_na(other_value) |>
  distinct() |>
  distinct(eudract_number, other_ids, other_value, .keep_all = TRUE) |>

  # Bring together name/number in the format: "name [number]"
  pivot_wider(names_from = other_type, values_from = other_value) |>
  mutate(other_numbers_protocol_unclean = str_c("[", other_numbers_protocol_unclean, "]")) |>
  unite(other_ids_protocol_unclean, other_names_protocol_unclean, other_numbers_protocol_unclean, sep = " ", na.rm = TRUE) |>
  mutate(other_ids_protocol_unclean = na_if(other_ids_protocol_unclean, "")) |>
  select(-row_id) |>

  # Bring together "name [number]" from across rows (i.e., country registrations) in the format: "name [number] | name [number]"
  mutate(other_ids_protocol_unclean = str_c(other_ids_protocol_unclean, collapse = " | "), .by = eudract_number) |>
  distinct() |>

  # Collapse other ids into semicolon-separated list
  mutate(other_ids = paste_rows(other_ids, collapse = " ; "), .by = eudract_number) |>
  distinct()


# Prepare cleaned protocol ------------------------------------------------

start <- Sys.time()

eu_protocol_clean <-
  eu_protocol_dump |>
  distinct(
    eudract_number,
    protocol_sponsor_code = sponsor_s_protocol_code_number,
    isrctn_number = isrctn_international_standard_randomised_controlled_trial_numbe,
    nct_number = us_nct_clinicaltrials_gov_registry_number,
    who_utn_number = who_universal_trial_reference_number_utrn
  ) |>

  # isrctn ------------------------------------------------------------------

  # Extract trn, if available
  ctregistries::mutate_trn_registry(by = isrctn_number) |>

  mutate(

    # ctregistries is "strict" and requires isrctn prefix
    # ctregistries::registries |> filter(registry == "ISRCTN") |> select(trn_regex)
    # "(?i)ISRCTN[[:blank:][:punct:]]*\\d{8}"
    # since isrctn field, use "lenient" regex without prefix
    trn =
      if_else(is.na(trn), str_extract(isrctn_number, "\\d{8}") %>% str_c("ISRCTN", .), trn),

    # If trn found, remove from unclean
    isrctn_number_protocol_unclean =
      if_else(!is.na(trn), str_remove_all(isrctn_number, trn) |> na_if(""), isrctn_number),

    # Clean any trns found
    isrctn_number = purrr::map_chr(trn, ctregistries::clean_trn),
  ) |>
  select(-registry, -trn) |>

  # ctgov -------------------------------------------------------------------

# Extract trn, if available
ctregistries::mutate_trn_registry(by = nct_number) |>

  mutate(

    # If trn found, remove from unclean
    nct_number_protocol_unclean =
      if_else(!is.na(trn), str_remove_all(nct_number, trn) |> na_if(""), nct_number),

    # Clean any trns found
    nct_number = purrr::map_chr(trn, ctregistries::clean_trn),
  ) |>
  select(-registry, -trn) |>
  distinct() |>

  # Add in cleaned other ids
  left_join(eu_protocol_other_ids, by = "eudract_number")

stop <- Sys.time()

runtime <- stop-start
