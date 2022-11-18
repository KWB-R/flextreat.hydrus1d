### Get Meteo Data from DWD


## Precipitation

install.packages("rdwd")
library(dplyr)
rdwd::updateRdwd()

rdwd::findID("Braunschweig")
rdwd::selectDWD(name = "Braunschweig", res = "daily")

url_bs_rain <- rdwd::selectDWD(name = "Braunschweig",
                               res = "hourly",
                               var = "precipitation",
                               per = "historical" )

bs_rain <- rdwd::dataDWD(url_bs_rain)

precipitation_hourly <- rdwd::dataDWD(url_bs_rain) %>%
  dplyr::select(.data$MESS_DATUM, .data$R1) %>%
  dplyr::rename("datetime" = "MESS_DATUM",
                "precipitation_mm" = "R1")

usethis::use_data(precipitation_hourly)


precipitation_daily <- precipitation_hourly %>%
  dplyr::mutate("date" = as.Date(datetime)) %>%
  dplyr::group_by(date) %>%
  dplyr::summarise(rain_mm = sum(precipitation_mm))

usethis::use_data(precipitation_daily)


## DWD datasets
remotes::install_github("kwb-r/kwb.dwd")


shape_file <- system.file("extdata/input-data/gis/Abwasserverregnungsgebiet.shp",
                          package = "flextreat.hydrus1d")


yearmonth_start <- "201611"
yearmonth_end <- "202204"

kwb.dwd:::list_daily_grids_germany_tgz("x")


# Only data of full months can currently be read!
dwd_daily_vars <- c("evaporation, potential" = "evapo_p",
                    "evaporation, real" = "evapo_r",
                    "soil moisture" = "soil_moist",
                    "soil moisture" = "soil_temperature_5cm")
dwd_daily_list <- stats::setNames(lapply(dwd_daily_vars, function(dwd_var) {
  kwb.dwd::read_daily_data_over_shape(
  file = shape_file,
  variable = dwd_var,
  from = yearmonth_start,
  to = yearmonth_end,
  quiet = TRUE
)
}), nm = dwd_daily_vars)


dwd_daily <- dplyr::bind_rows(dwd_daily_list, .id = "parameter")
"soil moisture" =


kwb.dwd:::list_monthly_grids_germany_asc_gz("x")


dwd_monthly_vars <- c("air temperature (mean)" = "air_temperature_mean",
  "drought index" = "drought_index",
  "evaporation, potential" = "evapo_p",
  "evaporation, real" = "evapo_r",
  "precipitation" = "precipitation",
  "soil moisture" = "soil_moist",
  "soil temperature (5 cm)" = "soil_temperature_5cm")

dwd_monthly_list <- stats::setNames(lapply(dwd_monthly_vars, function(dwd_var) {
  kwb.dwd::read_monthly_data_over_shape(
    file = shape_file,
    variable = dwd_var,
    from = yearmonth_start,
    to = yearmonth_end,
    quiet = TRUE
  )
}), nm = dwd_monthly_vars)


dwd_monthly <- dplyr::bind_rows(dwd_monthly_list, .id = "parameter")

dwd_monthly <- tibble::tibble(parameter_name = names(dwd_monthly_vars),
                              parameter = as.character(dwd_monthly_vars)) %>%
  dplyr::left_join(dwd_monthly)

usethis::use_data(dwd_monthly )


irrigation_file <- system.file("extdata/input-data/Beregnungsmengen_AVB.csv",
                          package = "flextreat.hydrus1d")


irrigation_file <- system.file("extdata/input-data/Beregnungsmengen_AVB.csv",
                               package = "flextreat.hydrus1d")


# irrigation_area <- rgdal::readOGR(dsn = shape_file)
# irrigation_area_sqm <- irrigation_area$area  # 44111068m2

## 2700ha (https://www.abwasserverband-bs.de/de/was-wir-machen/verregnung/)
irrigation_area_sqm <- 27000000

irrigation <- read.csv2(irrigation_file) %>%
  dplyr::select(- .data$Monat) %>%
  dplyr::rename(irrigation_m3 = .data$Menge_m3,
                source = .data$Typ,
                month = .data$Monat_num,
                year = .data$Jahr) %>%
  dplyr::mutate(date_start = as.Date(sprintf("%d-%02d-01",
                                             .data$year,
                                             .data$month)),
                days_in_month = as.numeric(lubridate::days_in_month(.data$date_start)),
                date_end =  as.Date(sprintf("%d-%02d-%02d",
                                            .data$year,
                                            .data$month,
                                            .data$days_in_month)),
                source = kwb.utils::multiSubstitute(.data$source,
                                                    replacements = list("Grundwasser" = "groundwater.mmPerDay",
                                                                        "Klarwasser" = "clearwater.mmPerDay")),
                irrigation_cbmPerDay = .data$irrigation_m3/.data$days_in_month,
                irrigation_area_sqm = irrigation_area_sqm,
                irrigation_mmPerDay = 1000*irrigation_cbmPerDay/irrigation_area_sqm) %>%
  dplyr::select(.data$year,
                .data$month,
                .data$days_in_month,
                .data$date_start,
                .data$date_end,
                .data$source,
                .data$irrigation_mmPerDay,
                .data$irrigation_area_sqm) %>%
  tidyr::pivot_wider(names_from = .data$source,
                     values_from = .data$irrigation_mmPerDay)

usethis::use_data(irrigation)

