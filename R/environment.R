# Install and load packages for R scripts -----------------------------------------------

cran_pkgs <- c(

  # config
  "ConfigParser",

  # General
  "dplyr", "tidyr", "stringr", "readr", "here", "tibble", "fs",

  # Testing
  "assertr"
)


to_install <- cran_pkgs[!cran_pkgs %in% installed.packages()]

if (length(to_install) > 0) {
  install.packages(to_install, deps = TRUE, repos = "https://cran.r-project.org")
}

# non-CRAN packages
if (!"unpaywallR" %in% installed.packages()) {
  devtools::install_github("NicoRiedel/unpaywallR")
}

invisible(lapply(c(cran_pkgs), library, character.only = TRUE))

# Specify configuration  -----------------------------------------------
cfg <- ConfigParser$new()
cfg$read("config.ini")

# Specify email
email_api <- cfg$get("email", NA, "login")
