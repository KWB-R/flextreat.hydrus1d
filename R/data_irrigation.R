#' Irrigation: Monthly
#'
#' Monthly irrigation values provided by AVB (in cubicmeters) downscaled to daily
#' values (by dividing with "days_in_month" and normalised to mm/squaremeter by
#' dividing with assumed irrigation area (44111068 m2)
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
#' install.packages(c("dplyr", "tidyr"))
#' irrigation_file <- system.file("extdata/Beregnungsmengen_AVB.csv",
#' package = "flextreat.hydrus1d")
#'
#' irrigation_area <- rgdal::readOGR(dsn = shape_file)
#'
#' irrigation <- read.csv2(irrigation_file) %>%
#'   dplyr::select(- .data$Monat) %>%
#'   dplyr::rename(irrigation_m3 = .data$Menge_m3,
#'                 source = .data$Typ,
#'                 month = .data$Monat_num,
#'                 year = .data$Jahr) %>%
#'   dplyr::mutate(date_start = as.Date(sprintf("%d-%02d-01",
#'                                              .data$year,
#'                                              .data$month)),
#'                 days_in_month = as.numeric(lubridate::days_in_month(.data$date_start)),
#'                 date_end =  as.Date(sprintf("%d-%02d-%02d",
#'                                             .data$year,
#'                                             .data$month,
#'                                             .data$days_in_month)),
#'                 source = kwb.utils::multiSubstitute(.data$source,
#'                                                     replacements = list("Grundwasser" = "groundwater.mmPerDay",
#'                                                                         "Klarwasser" = "clearwater.mmPerDay")),
#'                 irrigation_cbmPerDay = .data$irrigation_m3/.data$days_in_month,
#'                 irrigation_area_sqm = irrigation_area$area,
#'                 irrigation_mmPerDay = 1000*irrigation_cbmPerDay/irrigation_area_sqm) %>%
#'   dplyr::select(.data$year,
#'                 .data$month,
#'                 .data$days_in_month,
#'                 .data$date_start,
#'                 .data$date_end,
#'                 .data$source,
#'                 .data$irrigation_mmPerDay,
#'                 .data$irrigation_area_sqm) %>%
#'   tidyr::pivot_wider(names_from = .data$source,
#'                      values_from = .data$irrigation_mmPerDay)
#'}
#'head(flextreat.hydrus1d::irrigation)
#'
"irrigation"
