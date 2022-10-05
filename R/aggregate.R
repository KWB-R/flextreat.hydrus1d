#' Aggregate Atmosphere
#' @description for hydrologic years
#' @param atm_selected atm_selected as retrieved by \code{\link{prepare_atmosphere_data}}
#' @param format "wide" or "long
#'
#' @return aggregated data
#' @export
#'
#' @importFrom dplyr if_else n
#' @importFrom stringr str_replace
aggregate_atmosphere <- function(atm_selected, format = "wide") {

  cols_summarise <- names(atm_selected)[!names(atm_selected) %in% c("date", "irrigation_area_sqm")]

  atm_selected_hydro_wide <-  atm_selected %>%
    dplyr::mutate(hydrologic_year = get_hydrologic_years(.data$date)) %>%
    dplyr::group_by(.data$hydrologic_year) %>%
    dplyr::summarise(dplyr::across(.cols = tidyselect::all_of(cols_summarise),
                                   .fns = function(x) {sum(x, na.rm = TRUE)}),
                     n_days = dplyr::n())

  if(format == "wide") {
    return(atm_selected_hydro_wide)
  } else {
    atm_selected_hydro_wide %>%
      dplyr::select(- .data$n_days) %>%
      tidyr::pivot_longer(cols = tidyselect::all_of(cols_summarise),
                          names_to = "source", values_to = "value") %>%
      dplyr::mutate(source = stringr::str_remove(.data$source, "\\.mmPerDay$|_mean_mm$|_mm$") %>%
                      stringr::str_replace("clearwater$", "irrigation, clearwater") %>%
                      stringr::str_replace("groundwater$", "irrigation, groundwater") %>%
                      stringr::str_replace("evapo_p$", "evaporation, potential"),
                    value = dplyr::if_else(.data$source == "evaporation, potential",
                                           -.data$value,
                                           .data$value))
  }
}


#' Aggregate Solute
#'
#' @param solute solute as retrieved by \code{\link[kwb.hydrus1d]{read_solute}}
#' @param sim_datetime_start simulation start as datetime object (default:
#' as.POSIXct("2017-05-01", tz = "UTC"))
#' @param col_aggr column to be aggregated. One of "date", "yearmonth", "year" or
#' "year_hydrologic" (default: "date")
#' @return aggregated solute
#' @export
#' @importFrom dplyr group_by summarise relocate
aggregate_solute <- function(solute,
                             sim_datetime_start = as.POSIXct("2017-05-01", tz = "UTC"),
                             col_aggr = "date")  {
  solute %>%
    add_times(sim_datetime_start = sim_datetime_start) %>%
    dplyr::mutate(diff_time = c(0, diff(.data$time)),
                  time_c_top = .data$diff_time * .data$c_top,
                  time_c_bot = .data$diff_time * .data$c_bot,
                  time_integer = as.integer(.data$time)) %>%
    dplyr::group_by(.data[[col_aggr]]) %>%
    dplyr::summarise(diff_time = sum(.data$diff_time),
                     c_top = sum(.data$time_c_top)/.data$diff_time,
                     c_bot = sum(.data$time_c_bot)/.data$diff_time) %>%
    dplyr::relocate(.data[[col_aggr]])
}

#' Aggregate t_level
#'
#' @description  only for columns starting with "sum" and matching "volume"
#' @param t_level t_level as retrieved by \code{\link[kwb.hydrus1d]{read_tlevel}}
#' @param sim_datetime_start simulation start as datetime object (default:
#' as.POSIXct("2017-05-01", tz = "UTC"))
#' @param col_aggr column to be aggregated. One of "date", "yearmonth", "year" or
#' "year_hydrologic" (default: "date")
#' @return aggregated t_level data
#' @export
#' @importFrom tidyselect starts_with
#' @importFrom stats setNames
#' @importFrom stringr str_remove
#' @importFrom dplyr bind_rows
#' @importFrom tibble tibble
aggregate_tlevel <- function(t_level,
                             sim_datetime_start = as.POSIXct("2017-05-01", tz = "UTC"),
                             col_aggr = "date")  {
  tmp <- t_level %>%
    dplyr::select(.data$time,
                  tidyselect::starts_with("sum"),
                  tidyselect::starts_with("volume"))
  tmp$volume <- tmp$volume - tmp$volume[1]

  cols_to_drop <- names(tmp)[-1]

  t2 <- stats::setNames(lapply(cols_to_drop, function(col_name) {
    c(tmp[[col_name]][1], diff(tmp[[col_name]]))
  }),
  stringr::str_remove(cols_to_drop, "^sum_")) %>%
    dplyr::bind_cols()

  tmp2 <- dplyr::bind_cols(tmp[, "time"], t2)

  diff_time <- c(0,diff(tmp$time))

  tmp3 <- tibble::tibble(time = tmp$time,
                         diff_time = diff_time) %>%
    dplyr::bind_cols(t2)

  tmp3 %>%
    add_times(sim_datetime_start = sim_datetime_start) %>%
    dplyr::group_by(.data[[col_aggr]]) %>%
    dplyr::summarise(dplyr::across(tidyselect::all_of(names(tmp3)[-1]), ~ sum(.x))) %>%
    dplyr::relocate(.data[[col_aggr]])

}
