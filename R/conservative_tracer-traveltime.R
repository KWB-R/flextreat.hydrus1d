#' Helper function: interpolate time
#'
#' @param solute solute result tibble
#' @param percentiles (default: c(0.01, 0.05, 0.1, 0.25,0.5, 0.75, 0.9, 0.95, 0.99))
#'
#' @return tibble with interpolated times for percentiles
#' @export
#' @importFrom dplyr distinct
interpolate_time <- function(
    solute,
    percentiles = c(0.01, 0.05, 0.1, 0.25,0.5, 0.75, 0.9, 0.95, 0.99)
    ) {

  sum_cv_bot_values <- - percentiles*max(solute$sum_cv_top)
  sum_cv_top_values <- percentiles*max(solute$sum_cv_top)

  solute_unique_bot <- solute %>%
    dplyr::distinct(sum_cv_bot, .keep_all = TRUE)

  solute_unique_top <- solute %>%
    dplyr::distinct(sum_cv_top, .keep_all = TRUE)


  tibble::tibble(
  percentiles = percentiles,
  time_top = approx(solute_unique_top$sum_cv_top, solute_unique_top$time,
                    xout = sum_cv_top_values,
                    method = "linear", rule = 1)$y,
  time_bot = approx(solute_unique_bot$sum_cv_bot, solute_unique_bot$time,
         xout = sum_cv_bot_values,
         method = "linear", rule = 1)$y,
  time_diff = time_bot - time_top
  )

}

#' Helper function: get last days of months
#'
#' @param ids vector with ids
#' @param date_start (default: "2017-05-01")
#' @return
#' @keywords internal
#'
#' @importFrom lubridate %m+%
get_last_day_of_months <- function(ids,
                                   date_start = "2017-05-01") {
  sapply(ids, function(id) {
    start_date <- lubridate::ymd(date_start)
    month_date <- start_date %m+% months(id - 1)
    last_day <- lubridate::ceiling_date(month_date, "month") - days(1)

    return(last_day)
  }) %>% as.Date()
}


#' Get traveltimes (for conservative tracers)
#'
#' @param solute_files paths to solute files, with good naming convention for
#' monthly solute exposition (e.g. 0110, solute1: first month, solute10: tenth
#' month after simulation start)
#' @return tibble with time of substance at top/bottom and diff time. Note that
#' the percentile relate to the substance load
#' @export
#' @importFrom kwb.hydrus1d read_solute
#' @importFrom stringr str_remove_all
#' @importFrom dplyr left_join row_number
#'
get_traveltimes <- function(solute_files) {

solute_travel_list <- setNames(lapply(solute_files, function(path) {
    solute <- kwb.hydrus1d::read_solute(path)
    interpolate_time(solute)
    }), nm = solute_files)


solute_travel <- dplyr::bind_rows(solute_travel_list, .id = "solute_path") %>%
  dplyr::mutate(model_name = dirname(solute_path) %>% basename(),
                solute_id = stringr::str_remove_all(basename(solute_path), pattern = "solute|\\.out") %>%
                  as.integer())


lookup_model <- solute_travel %>%
  dplyr::count(model_name, solute_id) %>%
  dplyr::select(- n) %>%
  dplyr::mutate(month_id = dplyr::row_number(),
                date = get_last_day_of_months(month_id))


  dplyr::left_join(lookup_model, solute_travel,
                   by = c("model_name", "solute_id"))
}


#' Plot traveltimes
#'
#' @param solute_travel tibble as retrieved by \code{get_traveltimes}
#' @param title optional title (default: "")
#' @param ylim optional ylim (default: NULL)
#'
#' @return plotly of traveltimes for different percentiles
#' @export
#' @importFrom scales percent
#' @importFrom ggplot2 ggplot aes geom_point geom_line scale_color_discrete labs
#' theme_bw ylim
#' @importFrom plotly ggplotly
plot_traveltimes <- function(solute_travel, title = "", ylim = NULL) {

  percent_labels <- scales::percent(unique(solute_travel$percentiles))

  p1 <- solute_travel %>%
    ggplot2::ggplot(ggplot2::aes(x = date,
                                 y = time_diff,
                                 col = factor(percentiles,
                                              labels = percent_labels))) +
    ggplot2::geom_point() +
    ggplot2::geom_line() +
    ggplot2::scale_color_discrete(name = "Percentiles") +
    ggplot2::labs(title = title) +
    ggplot2::theme_bw()

  if(length(ylim == 2))  p1 <- p1 + ggplot2::ylim(ylim)


  plotly::ggplotly(p1)
}

