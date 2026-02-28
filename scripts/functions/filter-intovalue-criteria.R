# We often limit `trials` to trials meeting criteria per intovalue.
# These functions apply the appropriate filters.


# trials ------------------------------------------------------------------

read_iv_trials <- function(){

  # Read in trials
  trials_filepath <- here::here("data", "processed", "trials.csv")
  cli::cli_alert_info("Read in trials from {.file {trials_filepath}}")
  trials_raw <- readr::read_csv(trials_filepath, show_col_types = FALSE)

  cli::cli_alert_info("Trials filtered for intovalue criteria: completion date, status, interventional, German UMC lead, prefer IV2>1")

  # Apply the IntoValue exclusion criteria
  # https://github.com/quest-bih/clinical-dashboard/blob/main/prep/prep-intovalue.R#L9-L18
  trials <-

    trials_raw |>
    dplyr::filter(
      iv_completion,
      iv_status,
      iv_interventional,
      has_german_umc_lead,
      ## In case of dupes, exclude IV1 version
      !(is_dupe & iv_version == 1)
    ) |>
    # Check that trns are unique (duplicates removed)
    assertr::assert(assertr::is_uniq, id)

}


# cross-registrations -----------------------------------------------------

read_iv_cross_registrations <- function(){

  # Read in cross-registrations
  cross_registrations_filepath <- here::here("data", "processed", "trn", "cross-registrations.rds")
  cli::cli_alert_info("Read in cross-registrations from {.file {cross_registrations_filepath}}")
  cross_registrations_raw <- readr::read_rds(cross_registrations_filepath)

  # Read in trials (needed to filter)
  trials <- suppressMessages(read_iv_trials())

  cli::cli_alert_info("Cross-registrations filtered for intovalue")

  cross_registrations <-
    cross_registrations_raw |>

    # Remove exact duplicates (not sure why these appear)
    dplyr::distinct() |>

    # Limit to cross-registrations of trials meeting IntoValue exclusion criteria
    dplyr::semi_join(trials, by = "id") |>

    # Limit to cross-registrations found using IV2 paper
    dplyr::semi_join(trials, by = c("id", "doi"))

  stopifnot(

    # Check that no trn/doi's in trials and removed from crossreg
    trials |>
      dplyr::anti_join(cross_registrations, by = c("id", "doi")) |>
      dplyr::semi_join(cross_registrations_raw, y = _, by = c("id", "doi")) |>
      nrow() == 0,

    # Check that crossreg has no trn/doi's not in trials
    cross_registrations |>
      dplyr::anti_join(trials, by = c("id", "doi")) |>
      nrow() == 0
  )

  return(cross_registrations)

}
