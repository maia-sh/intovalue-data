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
