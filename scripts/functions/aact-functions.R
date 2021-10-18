#' Get AACT tables for given NCT ids
#'
#' Calls \code{connect_aact} to connect to AACT database and \code{query_aact} to query and download individual AACT tables. Dataframes saved to specified \code{dir} as .csv. If \code{tables} not specified, all tables (as prespecified in function and messaged to user) will be queried. Logs query date with \code{loggit}.
#'
#' AACT documentation available in the [database schema](https://aact.ctti-clinicaltrials.org/schema) and [data dictionary](https://aact.ctti-clinicaltrials.org/data_dictionary).

#' @param ids Character. Vector of NCT ids to query.
#' @param dir character, director in which to save XMLs. Defaults to working directory with `here::here()`.
#' @param user Character. AACT username.
#' @param password Character. Default is NULL. If not provided, password will be searched for in \code{keyring} under the \code{aact} service with the provided \code{username}. If no password found, user will be interactively asked and input will be stored in keyring.
#' @param tables Character. Vector of AACT tables to query. Default of `NULL` results in querying all tables ("studies", "designs", "interventions", "references", "ids", "centers", "officials", "responsible-parties", "sponsors", "facilities").
#' @param overwrite logical, if all tables already downloaded, should they be overwritten? Defaults to `FALSE`. Note that if *any* table is missing, *all* tables will be queried.
#'
#' @return NULL
#'
#' @export
#'
#' @example
#' \dontrun{
#' download_aact(ids = c("NCT00868166", "NCT00869726"),
#'               dir = fs::dir_create(here::here("data", "TEST")),
#'               user = "respmetrics")
#'}

download_aact <- function(ids,
                     dir = here::here(),
                     user, password = NULL,
                     tables = NULL,
                     overwrite = FALSE){

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
  QUERY <- "AACT"
  loggit::set_logfile(LOGFILE)

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

    rlang::inform(glue::glue("Already downloaded on {get_latest_query(QUERY)}: {tables_txt}"))
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
  loggit::loggit("INFO", QUERY)
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
