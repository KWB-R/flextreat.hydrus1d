#' Helper function: add times
#'
#' @param df df
#' @param sim_datetime_start simulation start as datetime object (default:
#' as.POSIXct("2017-05-01", tz = "UTC"))
#' @return data frame with added  times
#' @export
#' @importFrom lubridate days_in_month year month
add_times <- function(df,
                      sim_datetime_start = as.POSIXct("2017-05-01", tz = "UTC")) {
  df %>%
    dplyr::mutate(datetime = sim_datetime_start + .data$time*24*3600,
                  month_last_day = lubridate::days_in_month(.data$datetime),
                  year = lubridate::year(.data$datetime),
                  month = lubridate::month(.data$datetime),
                  year_hydrologic = get_hydrologic_years(.data$datetime),
                  yearmonth = as.Date(sprintf("%s-%02d",
                                              format(.data$datetime, format = "%Y-%m"),
                                              .data$month_last_day)),
                  date = as.Date(.data$datetime),
                  time_integer = as.integer(.data$time))
}
