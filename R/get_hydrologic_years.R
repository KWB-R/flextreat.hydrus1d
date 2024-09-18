#' Get Hydrologic Years
#'
#' @param datetime datetime string
#'
#' @return integer vector with hydrologic year
#' @export
#'
#' @importFrom lubridate year
#' @importFrom lubridate month
get_hydrologic_years <- function(datetime) {

years <- lubridate::year(datetime)
months <- lubridate::month(datetime)

hydro_years <-  ifelse(months >= 11, years, years - 1)

as.integer(hydro_years)

}
