#' Prepare Atmospheric Data
#'
#' @return data frame with atmospheric data for Braunschweig
#' @export
#' @importFrom dplyr left_join mutate select rename
#' @importFrom tibble tibble
#' @importFrom tidyselect all_of
#' @importFrom tidyr fill
#' @importFrom rlang .data
#' @examples
#' atm <- prepare_atmosphere_data()
#' atm
#'
prepare_atmosphere_data <- function() {

irri <- irrigation %>%
  dplyr::mutate(date = .data$date_end) %>%
  dplyr::select(- tidyselect::all_of(c("year", "month", "days_in_month", "date_start", "date_end")
  ))


tibble::tibble(date = seq(as.Date(min(irrigation$date_start)),
                                        as.Date(max(irrigation$date_end)),
                                        "days")) %>%
  dplyr::left_join(irri, by = "date") %>%
  tidyr::fill(.data$groundwater.mmPerDay,
              .data$clearwater.mmPerDay,
              .data$irrigation_area_sqm,
              .direction = "up") %>%
  dplyr::left_join(precipitation_daily,
                   by = "date") %>%
  dplyr::left_join(y = evapo_p %>%
                     dplyr::select(.data$date, .data$mean) %>%
                     dplyr::rename("evapo_p_mean_mm" = .data$mean),
                   by = "date")
}


#' Aggregate Atmospheric Data to Monthly Values
#'
#' @param atm atm as retrieved by \code{prepare_atmosphere_data}
#'
#' @return tibble with yearly atmospheric data values
#' @importFrom stringr str_extract
#' @importFrom dplyr select mutate group_by summarise
#' @export
#' @examples
#' create_monthly_atm()
#'
create_monthly_atm <- function(atm = prepare_atmosphere_data()) {
  atm %>%
    dplyr::select(- .data$irrigation_area_sqm) %>%
    dplyr::mutate(year = as.numeric(stringr::str_extract(.data$date, "\\d{4}")),
                  yearmonth = stringr::str_extract(.data$date, "\\d{4}-\\d{2}"),
                  days_in_month = as.numeric(lubridate::days_in_month(as.Date(paste0(.data$yearmonth, "-01")))),
                  date_end = as.Date(sprintf("%s-%s", .data$yearmonth, .data$days_in_month))
                  ) %>%
    dplyr::group_by(.data$date_end) %>%
    dplyr::summarise(rain_mm = sum(rain_mm, na.rm = TRUE),
                     irrigation_groundwater_mm = sum(groundwater.mmPerDay),
                     irrigation_clearwater_mm = sum(clearwater.mmPerDay),
                     potential_evaporation_mm = sum(evapo_p_mean_mm))

}


#' Plot Monthly Atmospheric Data
#'
#' @return plot of monthly atmospheric data
#' @export
#' @importFrom tidyr pivot_longer
#' @importFrom  ggplot2 aes ggplot geom_point labs theme theme_bw
#' @examples
#' plot_monthly_atm()
plot_monthly_atm <- function() {

atm_tidy <-  create_monthly_atm() %>%
    tidyr::pivot_longer(names_to = "key",
                        values_to = "value",
                        cols = - .data$date_end)

atm_tidy %>%
  ggplot2::ggplot(mapping = ggplot2::aes(x = .data$date_end,
                                         y = .data$value,
                                         col = .data$key)) +
  ggplot2::geom_point() +
  ggplot2::labs(title="",
                x ="",
                y = "mm/month") +
  ggplot2::theme(legend.position="top") +
  ggplot2::theme_bw()
}
