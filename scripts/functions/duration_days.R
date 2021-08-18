# Helper function to calculate duration in days
duration_days <- function(start, end){
  as.numeric(lubridate::as.duration(lubridate::interval(start, end)), "days")
}
