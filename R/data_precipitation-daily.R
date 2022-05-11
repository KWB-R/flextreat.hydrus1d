#' Precipitation: Daily
#'
#' Hourly precipitation data downloaded from DWD for monitoring station Braunschweig
#' (id = 662) between 1997-10-22 and 2021-12-31, which were aggregated to daily
#' values within R
#'
#' @format A data.frame with 8835 rows and 3 variables:
#' \describe{
#'   \item{year}{year}
#'   \item{month}{month}
#'   \item{days_in_month}{days in month}
#'   \item{date_start}{date start}
#'   \item{date_end}{date end}
#'   \item{irrigation_area_sqm}{irrigation area in squaremeter}
#'   \item{"groundwater.mmPerDay}{irrigation using "groundwater" (mm/sqm)}
#'   \item{"clearwater.mmPerDay}{irrigation using "clearwater" (mm/sqm)}
#'   )}
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
#'precipitation_daily <- precipitation_hourly %>%
#'  dplyr::mutate("date" = as.Date(datetime)) %>%
#'  dplyr::group_by(date) %>%
#'  dplyr::summarise(rain_mm = sum(precipitation_mm))
#'
#'}
#'#'head(flextreat.hydrus1d::precipitation_daily)
"precipitation_daily"
