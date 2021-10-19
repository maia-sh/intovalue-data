#' Get AACT tables for given NCT ids
#'
#' Calls \code{connect_aact} to connect to AACT database and \code{query_aact} to query and download individual AACT tables. Dataframes saved to specified \code{dir} as .csv. If \code{tables} not specified, all tables (as prespecified in function and messaged to user) will be queried. Logs query date with \code{loggit}.
#'
#' AACT documentation available in the [database schema](https://aact.ctti-clinicaltrials.org/schema) and [data dictionary](https://aact.ctti-clinicaltrials.org/data_dictionary).

#' @param ids Character. Vector of NCT ids to query.
#' @param dir Character. Directory in which to save raw CSVs. Defaults to working directory with `here::here("raw")`.
#' @param user Character. AACT username.
#' @param password Character. Default is NULL. If not provided, password will be searched for in \code{keyring} under the \code{aact} service with the provided \code{username}. If no password found, user will be interactively asked and input will be stored in keyring.
#' @param tables Character. Vector of AACT tables to query. Default of `NULL` results in querying all tables ("studies", "designs", "interventions", "references", "ids", "centers", "officials", "responsible-parties", "sponsors", "facilities").
#' @param overwrite Logical. If all tables already downloaded, should they be overwritten? Defaults to `FALSE`. Note that if *any* table is missing, *all* tables will be queried.
#' @param query Character. Query `INFO` for \code{loggit}. Defaults to "AACT".
#'
#' @return NULL
#'
#' @export
#'
#' @example
#' \dontrun{
#' download_aact(ids = c("NCT00868166", "NCT00869726"),
#'               dir = here::here("data", "TEST", "raw"),
#'               user = "respmetrics")
#'}

download_aact <- function(ids,
                     dir = here::here("raw"),
                     user, password = NULL,
                     tables = NULL,
                     overwrite = FALSE,
                     query = "AACT"){

  # Queries prepared for certain tables
  valid_tables <- c("studies", "designs", "interventions", "references", "ids", "centers", "officials", "responsible-parties", "sponsors", "facilities")

  # If no user-specified tables, query all tables
  if (rlang::is_null(tables)) tables <- valid_tables

  tables_txt <-
    glue::glue_collapse(glue::glue("'{tables}'"), sep = ", ", last = ", and ")

  # Check for valid tables
  if (any(!tables %in% valid_tables)){
    rlang::abort(glue::glue("All `tables` must be in {tables_txt}"))
  }

  # Prepare log
  LOGFILE <- here::here("queries.log")
  loggit::set_logfile(LOGFILE)


  # Create output directory if doesn't exist
  fs::dir_create(dir)

  # If all tables already downloaded and not overwriting, then inform user and return
  downloaded_tables <-
    fs::dir_ls(dir) %>%
    fs::path_file() %>%
    fs::path_ext_remove()

  if (all(tables %in% downloaded_tables) & !overwrite){

    # Function to get latest query
    get_latest_query <- function(query, logfile) {

      # Read logs and catch error if no existing logs
      logs <- tryCatch(error = function(cnd) NULL, loggit::read_logs(logfile))

      if (!rlang::is_null(logs)){
        logs %>%
          dplyr::filter(log_msg == query) %>%
          dplyr::arrange(dplyr::desc(timestamp)) %>%
          dplyr::slice_head(n = 1) %>%
          dplyr::pull(timestamp) %>%
          as.Date.character()
      } else {"No previous query"}
    }

    rlang::inform(glue::glue("Already downloaded on {get_latest_query(query)}: {tables_txt}"))
    return(NULL)
  }

  # Inform user about tables to be queried
  rlang::inform(glue::glue("Querying AACT for tables: {tables_txt}"))

  con <- connect_aact(user, password = password)

  # Query aact database and write output to files
  if ("studies" %in% tables){
    query_aact(
      "studies", ids,
      con, filepath = fs::path(dir, "studies", ext = "csv"),
      last_update_submitted_date,
      start_month_year, completion_month_year, primary_completion_month_year,
      study_first_submitted_date, results_first_submitted_date,
      study_type, phase, enrollment, overall_status,
      official_title, brief_title
    )
  }

  if ("designs" %in% tables){
    query_aact(
      "designs", ids,
      con, filepath = fs::path(dir, "designs", ext = "csv"),
      allocation, masking
    )
  }

  if ("interventions" %in% tables){
    query_aact(
      "interventions", ids,
      con, filepath = fs::path(dir, "interventions", ext = "csv"),
      intervention_type
    )
  }

  if ("references" %in% tables){
    query_aact(
      "study_references", ids,
      con, filepath = fs::path(dir, "references", ext = "csv"),
      pmid, reference_type, citation
    )
  }

  if ("ids" %in% tables){
    query_aact(
      "id_information", ids,
      con, filepath = fs::path(dir, "ids", ext = "csv"),
      id_type, id_value
    )
  }

  if ("centers" %in% tables){
    query_aact(
      "calculated_values", ids,
      con, filepath = fs::path(dir, "centers", ext = "csv"),
      has_single_facility, number_of_facilities
    )
  }

  if ("officials" %in% tables){
    query_aact(
      "overall_officials", ids,
      con, filepath = fs::path(dir, "officials", ext = "csv"),
      affiliation
    )
  }

  if ("responsible-parties" %in% tables){
    query_aact(
      "responsible_parties", ids,
      con, filepath = fs::path(dir, "responsible-parties", ext = "csv"),
      affiliation, organization
    )
  }

  if ("sponsors" %in% tables){
    query_aact(
      "sponsors", ids,
      con, filepath = fs::path(dir, "sponsors", ext = "csv"),
      agency_class, # main_sponsor
      lead_or_collaborator, name
    )
  }

  if ("facilities" %in% tables){
    query_aact(
      "facilities", ids,
      con, filepath = fs::path(dir, "facilities", ext = "csv"),
      name, city, country
    )
  }

  # Disconnect aact database
  RPostgreSQL::dbDisconnect(con)

  # Log query date
  loggit::loggit("INFO", query)
}

# Get clinicaltrials.gov data via https://aact.ctti-clinicaltrials.org/
# AACT is a relational database of clinicaltrials.gov

# You must first create an account with AACT: https://aact.ctti-clinicaltrials.org/users/sign_up

#' Connect to AACT database
#'
#' Based on https://aact.ctti-clinicaltrials.org/r.
#' Use account credentials created via https://aact.ctti-clinicaltrials.org/users/sign_up
#'
#' @param user Character. AACT username.
#' @param password Character. Default is NULL. If not provided, password will be searched for in \code{keyring} under the \code{aact} service with the provided \code{username}. If no password found, user will be interactively asked and input will be stored in keyring.
#'
#' @return <PostgreSQLConnection> object from \code{dbConnect}
#'

connect_aact <- function(user, password = NULL) {
  password <-
    ifelse(
      rlang::is_true(stringr::str_detect(keyring::key_list("aact")$username, user)),
      keyring::key_get("aact", user),
      keyring::key_set("aact", user)
    )

  drv <- DBI::dbDriver('PostgreSQL')
  con <- RPostgreSQL::dbConnect(drv,
                   dbname = "aact",
                   host = "aact-db.ctti-clinicaltrials.org",
                   port = 5432,
                   user = user,
                   password = password
  )

  con
}


#' Query AACT table
#'
#' AACT documentation available in the [database schema](https://aact.ctti-clinicaltrials.org/schema) and [data dictionary](https://aact.ctti-clinicaltrials.org/data_dictionary)
#'
#' @param table Character. AACT table.
#' @param ids Character. Vector of NCT ids to query.
#' @param con Connection to AACT database as returned by \code{dbConnect}.
#' @param filepath Character. Filepath including directory portion and ending in ".csv". If no filepath provided, output will be only returned and not written to file.
#' @param ... One or more unquoted expressions separated by commas. Column names from AACT table. `nct_id` is always returned and should not be included in this.
#'
#' @return Dataframe with specified columns from \code{table}
#'

query_aact <- function(table, ids, con, filepath = NULL, ...){

  query <-
    dplyr::tbl(con, table) %>%
    dplyr::filter(nct_id %in% ids) %>%
    dplyr::select(
      nct_id,
      ...
    )

  # if (!quiet) dplyr::show_query(query)

  out <- dplyr::collect(query)

  if (!rlang::is_null(filepath)){
    readr::write_csv(out, filepath)
  }

  out
}

#' Process AACT tables in given directory, process into rds
#'
#' Processes and cleans AACT tables from \code{dir_raw}. Dataframes saved to specified \code{dir_out} as .rds. All tables (as prespecified in function and matching \code{download_aact}) will be processed unless *all* already processed and \code{overwrite} is `FALSE`.
#'
#' AACT documentation available in the [database schema](https://aact.ctti-clinicaltrials.org/schema) and [data dictionary](https://aact.ctti-clinicaltrials.org/data_dictionary).

#' @param dir_in Character. Directory from which to get raw .csv files. Defaults to working directory with `here::here("raw")`.
#' #' @param dir_out Character. Directory in which to save processed .rds files. Defaults to working directory with `here::here("processed")`.
#' @param overwrite Logical. If all tables already processed, should they be overwritten? Defaults to `FALSE`. Note that if *any* table is not processed, *all* tables will be processed.
#'
#' @return NULL
#'
#' @export
#'
#' @example
#' \dontrun{
#' process_aact(dir_in = here::here("data", "TEST", "raw"),
#'              dir_out = here::here("data", "TEST", "processed"))
#'}



process_aact <- function(dir_in = here::here("raw"),
                         dir_out = here::here("processed"),
                         # tables = NULL, # could add param to chose tables
                         overwrite = FALSE){

  # Create output directory if doesn't exist
  fs::dir_create(dir_out)

  # Processing prepared for certain tables
  valid_tables <- c("studies", "designs", "interventions", "references", "ids", "centers", "officials", "responsible-parties", "sponsors", "facilities")

  # If all tables already processed and not overwriting, then inform user and return
  raw_tables <-
    fs::dir_ls(dir_in) %>%
    fs::path_file() %>%
    fs::path_ext_remove() %>%

    # Limit to valid tables in case of additional files
    intersect(valid_tables)

  processed_tables <-
    fs::dir_ls(dir_out) %>%
    fs::path_file() %>%
    fs::path_ext_remove()

  unprocessed_tables <- setdiff(raw_tables, processed_tables)

  if (rlang::is_empty(unprocessed_tables) & !overwrite){

    rlang::inform(glue::glue("Already downloaded all tables: ", glue::glue_collapse(unprocessed_tables, sep = ", ", last = ", and ")))
    return(NULL)

  }

  # If not all tables processed, reprocess *all* tables (alternatively could process unprocessed only)

  # Process lead and facility affiliations ----------------------------------

  # Since we don't distinguish between sponsors/official/responsible parties, combine affiliations
  # Note: For now, using intovalue umc affiliations, so simply save output

  # Check that all three lead affiliation tables available
  if (all(c("responsible-parties", "officials", "sponsors") %in% raw_tables)){
    resp_parties <-
      readr::read_csv(fs::path(dir_in, "responsible-parties", ext = "csv")) %>%
      dplyr::filter(!is.na(affiliation) | !is.na(organization)) %>%

      # Check that each study has only affiliation OR organization, and then merge
      assertr::assert_rows(assertr::num_row_NAs, assertr::in_set(1), c(affiliation, organization)) %>%

      dplyr::mutate(affiliation = dplyr::coalesce(affiliation, organization), .keep = "unused") %>%
      dplyr::distinct() %>%
      dplyr::mutate(affiliation_type = "Responsible Party")

    officials <-
      readr::read_csv(fs::path(dir_in, "officials", ext = "csv")) %>%
      dplyr::distinct() %>%
      dplyr::filter(!is.na(affiliation)) %>%
      dplyr::mutate(affiliation_type = "Study Official")

    sponsors <-
      readr::read_csv(fs::path(dir_in, "sponsors", ext = "csv")) %>%
      dplyr::filter(lead_or_collaborator == "lead") %>%
      dplyr::select(-lead_or_collaborator) %>%

      dplyr::rename(
        main_sponsor = agency_class,
        affiliation = name
      ) %>%
      assertr::assert(assertr::is_uniq, nct_id) %>%
      dplyr::mutate(affiliation_type = "Sponsor")

    affiliations <-
      dplyr::bind_rows(sponsors, resp_parties, officials) %>%
      dplyr::distinct() %>%
      dplyr::select(nct_id, affiliation_type, lead_affiliation = affiliation) %>%
      dplyr::arrange(nct_id)

    readr::write_rds(affiliations, fs::path(dir_out, "ctgov-lead-affiliations", ext = "rds"))

  }

  if ("facilities" %in% raw_tables){
    facilities <-
      readr::read_csv(fs::path(dir_in, "facilities", ext = "csv")) %>%
      dplyr::rename(facility_affiliation = name)

    readr::write_rds(facilities, fs::path(dir_out, "ctgov-facility-affiliations", ext = "rds"))

  }

  # Process studies ---------------------------------------------------------

  if ("studies" %in% raw_tables){
    studies <-
      readr::read_csv(fs::path(dir_in, "studies", ext = "csv")) %>%

      dplyr::rename(
        registration_date = study_first_submitted_date,
        summary_results_date = results_first_submitted_date,
        recruitment_status = overall_status
      ) %>%

      # Parse dates, which are either Month Year, or Month Day Year
      # If no day, default to 1st, like intovalue
      dplyr::mutate(
        start_date = lubridate::parse_date_time(start_month_year, c("my", "mdY")),
        completion_date = lubridate::parse_date_time(completion_month_year, c("my", "mdY")),
        primary_completion_date = lubridate::parse_date_time(primary_completion_month_year, c("my", "mdY")),
        .keep = "unused"
      ) %>%

      dplyr::mutate(
        has_summary_results = dplyr::if_else(!is.na(summary_results_date), TRUE, FALSE)
      ) %>%
      #
      # mutate(
      #   days_reg_to_start = duration_days(registration_date, start_date),
      #   days_reg_to_comp = duration_days(registration_date, completion_date),
      #   days_comp_to_summary = duration_days(completion_date, summary_results_date)
      # ) %>%

      dplyr::mutate(
        phase = dplyr::na_if(phase, "N/A"),
        study_type =
          dplyr::if_else(
            stringr::str_detect(study_type, "Observational"),
            "Observational", study_type
          )
      ) %>%

      dplyr::rename(title = brief_title) %>%

      dplyr::select(-official_title)
  }

  if (all(c("designs", "centers", "sponsors") %in% raw_tables)){

    designs <- readr::read_csv(fs::path(dir_in, "designs", ext = "csv"))
    centers <- readr::read_csv(fs::path(dir_in, "centers", ext = "csv"))

    studies <-

      studies %>%

      # Check that designs & centers have no duplicates per study, and add to studies
      dplyr::left_join(assertr::assert(designs, assertr::is_uniq, nct_id), by = "nct_id") %>%
      dplyr::left_join(assertr::assert(centers, assertr::is_uniq, nct_id), by = "nct_id") %>%
      dplyr::left_join(dplyr::select(sponsors, nct_id, main_sponsor), by = "nct_id") %>%

      dplyr::mutate(
        allocation =  dplyr::na_if(allocation, "N/A"),
        is_multicentric = !has_single_facility
      ) %>%

      dplyr::select(-has_single_facility, -number_of_facilities)

  }

  readr::write_rds(studies, fs::path(dir_out, "ctgov-studies", ext = "rds"))

  # Process interventions ---------------------------------------------------

  # Note: Unclear how IntoValue selects single intervention_type, so for now disregard

  # if ("interventions" %in% raw_tables){
  #
  #   interventions <- readr::read_csv(fs::path(dir_in, "interventions", ext = "csv"))
  #
  #   interventions %>%
  #     dplyr::distinct() %>%
  #     janitor::get_dupes(nct_id) %>%
  #     dplyr::count(dupe_count)
  #
  #   interventions %>%
  #     dplyr::distinct() %>%
  #     dplyr::group_by(nct_id) %>%
  #     dplyr::mutate(intervention_types = paste(intervention_type, collapse="; ")) %>%
  #     dplyr::distinct(nct_id, intervention_types)
  # }

  # Process ids -------------------------------------------------------------
  if ("ids" %in% raw_tables){
    ids <-
      readr::read_csv(fs::path(dir_in, "ids", ext = "csv")) %>%

      ctregistries::mutate_trn_registry(id_value) %>%

      # Clean trns and collapse EudraCT entries
      dplyr::mutate(
        raw_trn = trn,
        trn = purrr::map_chr(raw_trn, ctregistries::clean_trn)
      )

    readr::write_rds(ids, fs::path(dir_out, "ctgov-ids", ext = "rds"))

    crossreg <-
      ids %>%
      dplyr::filter(!is.na(trn)) %>%
      dplyr::select(nct_id, crossreg_registry = registry, crossreg_trn = trn)

    readr::write_rds(crossreg, fs::path(dir_out, "ctgov-crossreg", ext = "rds"))
  }

  # Process references ------------------------------------------------------

  if ("references" %in% raw_tables){
    references <-
      readr::read_csv(fs::path(dir_in, "references", ext = "csv")) %>%

      # Extract identifiers
      dplyr::mutate(
        pmid = as.numeric(pmid),
        doi = stringr::str_extract(citation, "10\\.\\d{4,9}/[-.;()/:\\w\\d]+"),
        doi = stringr::str_remove(doi, "\\.$"), # remove trailing period
        pmcid = stringr::str_extract(citation, "PMC[0-9]{7}")
      ) %>%

      # Some references are automatically derived in ct.gov
      dplyr::mutate(reference_derived = dplyr::if_else(reference_type == "derived", TRUE, FALSE))

    readr::write_rds(references, fs::path(dir_out, "ctgov-references", ext = "rds"))
  }
}
