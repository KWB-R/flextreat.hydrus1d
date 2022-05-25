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


## Potential Evaporation
remotes::install_github("kwb-r/kwb.dwd@dev")


shape_file <- system.file("extdata/input-data/gis/Abwasserverregnungsgebiet.shp",
                          package = "flextreat.hydrus1d")

# Only data of full months can currently be read!
evapo_p <- kwb.dwd::read_daily_data_over_shape(
  file = shape_file,
  variable = "evapo_p",
  from = "201701",
  to = "202012"
)

usethis::use_data(evapo_p)


irrigation_file <- system.file("extdata/input-data/Beregnungsmengen_AVB.csv",
                          package = "flextreat.hydrus1d")


irrigation_area <- rgdal::readOGR(dsn = shape_file)


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
                irrigation_area_sqm = irrigation_area$area,
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

