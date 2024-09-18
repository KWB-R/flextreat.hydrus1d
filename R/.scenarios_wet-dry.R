atm <- flextreat.hydrus1d::prepare_atmosphere_data() %>%
dplyr::mutate(hydrologic_year = flextreat.hydrus1d::get_hydrologic_years(date),
              year = as.integer(format(date, format = "%Y")),
              day_of_year = lubridate::yday(date))


atm_stats <- atm %>%
  dplyr::group_by(year) %>%
  dplyr::summarise(rain_mm = sum(rain_mm, na.rm = TRUE),
                   evapo_p_mean_mm = sum(evapo_p_mean_mm, na.rm = TRUE))


atm_wet <- atm_stats %>%
  dplyr::filter(rain_mm == max(rain_mm))


atm_wet_sel <- atm %>%
  dplyr::filter(year == atm_wet$year) %>%
  dplyr::select(day_of_year, rain_mm)

atm_wet_input <- atm %>%
  dplyr::select(- rain_mm, - hydrologic_year, - year) %>%
  dplyr::left_join(atm_wet_sel) %>%
  dplyr::mutate(rain_mm = dplyr::if_else(is.na(rain_mm), 0, rain_mm)) %>%
  dplyr::select(- day_of_year) %>%
  dplyr::relocate(rain_mm, .after = clearwater.mmPerDay)


atm_dry <- atm_stats %>%
  dplyr::filter(rain_mm == min(rain_mm))

atm_dry_sel <- atm %>%
  dplyr::filter(year == atm_dry$year) %>%
  dplyr::select(day_of_year, rain_mm)

atm_dry_input <- atm %>%
  dplyr::select(- rain_mm, - hydrologic_year, - year) %>%
  dplyr::left_join(atm_dry_sel) %>%
  dplyr::mutate(rain_mm = dplyr::if_else(is.na(rain_mm), 0, rain_mm)) %>%
  dplyr::select(- day_of_year) %>%
  dplyr::relocate(rain_mm, .after = clearwater.mmPerDay)
