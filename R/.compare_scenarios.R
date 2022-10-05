if (FALSE) {
with_irrigation <- readRDS(file = "vignettes/wb_yearmonth_status-quo.Rds")
no_irrigation <- readRDS(file = "vignettes/wb_yearmonth_status-quo_no-irrigation.Rds")

with_irrigation
no_irrigation

get_para <- function(df, para = "v_bot") {


df_name <- deparse(substitute(df))


tmp <- df %>%
  dplyr::filter(.data$parameter == para) %>%
  dplyr::select(- .data$parameter,
                - .data$diff_time)

names(tmp)[names(tmp) == "value"] <- sprintf("%s_%s", para, df_name)

tmp
}


with_irrigation <- with_irrigation$data
no_irrigation <- no_irrigation$data

plot_recharge_comparison <- function(col_aggr = "yearmonth",
                                     y_label = "Groundwater Recharge (mm/month)") {


inf_wide <- get_para(with_irrigation) %>%
  dplyr::left_join(get_para(no_irrigation)) %>%
  dplyr::mutate(#v_bot_diff = round(.data$v_bot_no_irrigation - .data$v_bot_with_irrigation,1),
                hydro_year = flextreat.hydrus1d::get_hydrologic_years(.data$yearmonth)) %>%
  dplyr::group_by(.data[[col_aggr]]) %>%
  dplyr::summarise(dplyr::across(tidyselect::starts_with("v_bot"), sum))

inf_plot <- inf_wide %>%
  tidyr::pivot_longer(- .data[[col_aggr]]) %>%
  dplyr::mutate(value = -.data$value,
                name = stringr::str_remove(.data$name, pattern = "v_bot_")) %>%
  ggplot2::ggplot(mapping = ggplot2::aes(x = .data[[col_aggr]],
                                         y = .data$value,
                                         col = .data$name)) +
  ggplot2::scale_color_discrete(name = "") +
  ggplot2::geom_point() +
  ggplot2::geom_line() +
  ggplot2::labs(y = y_label,
                x = "") +
  ggplot2::theme_bw() +
  ggplot2::theme(legend.position = "top")


inf_plot
}


kwb.utils::preparePdf("gw-recharge_comparison.pdf",
                      width.cm = 19,
                      height.cm = 10)
plot_recharge_comparison("yearmonth")
plot_recharge_comparison("hydro_year")
dev.off()

get_para(with_irrigation, "evap") %>%
  dplyr::left_join(get_para(no_irrigation, "evap")) %>%
  dplyr::mutate(infil_diff = round(.data$evap_with_irrigation - .data$evap_no_irrigation,1),
                hydro_year = flextreat.hydrus1d::get_hydrologic_years(.data$yearmonth)) %>%
  dplyr::group_by(.data$hydro_year) %>%
  dplyr::summarise(dplyr::across(tidyselect::starts_with("evap"), sum))


}
