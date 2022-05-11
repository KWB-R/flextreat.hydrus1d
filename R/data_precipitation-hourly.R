#' Precipitation: Hourly
#'
#' Hourly precipitation data downloaded from DWD for monitoring station Braunschweig
#' (id = 662) between 1997-10-22 and 2021-12-31
#'
#' @format A data.frame with 211629 rows and 2 variables:
#' \describe{
#'   \item{datetime}{date time}
#'   \item{precipitation_mm}{precipitation in mm}
#' }
#' @examples
#' \dontrun{
#' install.packages(c("dplyr", "rdwd"))
#' library(dplyr)
#'rdwd::updateRdwd()
#'rdwd::findID("Braunschweig")
#'rdwd::selectDWD(name = "Braunschweig", res = "daily")
#'
#'url_bs_rain <- rdwd::selectDWD(name = "Braunschweig",
#'                               res = "hourly",
#'                               var = "precipitation",
#'                               per = "historical" )
#'
#'bs_rain <- rdwd::dataDWD(url_bs_rain)
#'
#'precipitation_hourly <- rdwd::dataDWD(url_bs_rain) %>%
#'  dplyr::select(.data$MESS_DATUM, .data$R1) %>%
#'  dplyr::rename("datetime" = "MESS_DATUM",
#'                "precipitation_mm" = "R1")
#'
#'}
"precipitation_hourly"
