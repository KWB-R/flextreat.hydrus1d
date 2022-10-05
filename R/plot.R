#' Plot Atmosphere
#'
#' @param atm_selected_hydro_long as retrieved by \code{\link{aggregate_atmosphere}}
#' in "long" format
#'
#' @return plot atmosphere
#' @export
#' @importFrom ggplot2 geom_col scale_fill_discrete theme_bw theme aes ggplot
#'
plot_atmosphere <- function(atm_selected_hydro_long) {

  atm_selected_hydro_long %>%
    ggplot2::ggplot(mapping = ggplot2::aes(x = .data$hydrologic_year,
                                           y = .data$value,
                                           fill = .data$source)) +
    ggplot2::geom_col() +
    ggplot2::scale_fill_discrete(name = "") +
    ggplot2::labs(x = "Hydrologic Year",
                  y = "Value (mm/a)") +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "top")

}


#' Plot Solute
#'
#' @param solute_aggr solute_aggr as retrieved by \code{\link{aggregate_solute}}
#' @param y_label y_label (default: "Share of 'clearwater' (%)")
#'
#' @return plot solute
#' @export
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr filter
#' @importFrom ggplot2 aes ggplot geom_line geom_point scale_color_discrete
#' scale_y_continuous labs theme_bw theme
#' @importFrom scales percent_format

plot_solute <- function(solute_aggr, y_label = "Share of 'clearwater' (%)") {

  x_col <- names(solute_aggr)[1]

  solute_aggr %>%
    tidyr::pivot_longer(cols = c("c_top", "c_bot"),
                        names_to = "parameter",
                        values_to = "value") %>%
    dplyr::filter(!is.na(.data$value)) %>%
    ggplot2::ggplot(mapping = ggplot2::aes(x = .data[[x_col]],
                                           y = .data$value,
                                           col = .data$parameter )) +
    ggplot2::geom_point() +
    ggplot2::geom_line() +
    ggplot2::scale_color_discrete(name = "Parameter") +
    ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                                limits = c(0,1)) +
    ggplot2::labs(y = y_label) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "top")
}


#' Plot Water Balance
#'
#' @param tlevel_aggr aggregated t_level as retrieved by \code{\link{aggregate_tlevel}}
#' @param y_label default: "Water Balance Component (mm)"
#' @param unit_org original unit in "t_level" (default: "cm")
#' @param unit_target target unit for plot (default: "mm")
#'
#' @return plot water balance
#' @export
#' @importFrom tidyselect all_of
#' @importFrom measurements conv_unit
#' @importFrom dplyr filter
#' @importFrom tidyr pivot_longer
#' @importFrom ggplot2 labs ggplot aes scale_color_discrete theme_bw theme
#' geom_point geom_line
plot_waterbalance <- function(tlevel_aggr,
                              y_label = "Water Balance Component",
                              unit_org = "cm",
                              unit_target = "mm") {

  x_col <- names(tlevel_aggr)[1]

  y_label <- sprintf("%s (%s)", y_label, unit_target)

  tlevel_aggr %>%
    tidyr::pivot_longer(cols = tidyselect::all_of(names(tlevel_aggr)[c(-1, -2)]),
                        names_to = "parameter",
                        values_to = "value") %>%
    dplyr::mutate(value = measurements::conv_unit(.data$value,
                                                  from = unit_org,
                                                  to = unit_target)) %>%
    dplyr::filter(.data$parameter %in% c("infil", "evap", "run_off", "v_bot", "volume")) %>%
    ggplot2::ggplot(mapping = ggplot2::aes(x = .data[[x_col]],
                                           y = .data$value,
                                           col = .data$parameter )) +
    ggplot2::scale_color_discrete(name = "Component") +
    ggplot2::labs(y = y_label) +
    ggplot2::geom_point() +
    ggplot2::geom_line() +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "top")
}

