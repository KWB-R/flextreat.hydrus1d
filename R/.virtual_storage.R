remotes::install_github("kwb-r/kwb.hydrus1d@dev")
remotes::install_github("kwb-r/flextreat.hydrus1d@dev")

#_no-irrig

scenarios <- c("soil-1.5m", "soil-2m", "soil-1.5m_no-irrig", "soil-2m_no-irrig")
scenarios <- c("soil-1m", "soil-3m", "soil-1m_no-irrig", "soil-3m_no-irrig")
scenarios <- c("soil-1m_no-irrig", "soil-3m_no-irrig")
scenarios <- c("soil-3m_no-irrig")

scenarios <- c("soil-3m", "soil-3m_no-irrig")

soil_depths <- c(1, 1.5, 2, 3)

scenarios <- sapply(soil_depths, function(x) {
  c(sprintf("soil-%sm", as.character(x)),
    sprintf("soil-%sm_no-irrig", as.character(x)))}) %>% as.vector()

# org <- fs::dir_ls(paths$exe_dir, regexp = "soil-[1|3]m", type = "directory")
# new <- stringr::str_replace(org, pattern = "m_", replacement = "m_no-irrig_")
# fs::dir_copy(org, new)
# org_h1d <- file.path(paths$exe, "1a2a_soil-2m_tracer_0110.h1d")
# new_h1d <- sprintf("%s.h1d", org)
# sapply(new_h1d, function(x) fs::file_copy(org_h1d, x))

periods <- tibble::tibble(start = c("01", "11", "21", "31"),
                          end = c("10", "20", "30", "40"))

sapply(scenarios, function(scenario) {

sapply(seq_len(nrow(periods)), function(i) {

paths_list <- list(
  #extdata = system.file("extdata", package = "flextreat.hydrus1d"),
  #root_server = "Y:/WWT_Department/Projects/FlexTreat/Work-packages/AP3/3_1_4_Prognosemodell/Vivian/Rohdaten/H1D",
  root_local = "C:/kwb/projects/flextreat/3_1_4_Prognosemodell/Vivian/Rohdaten/H1D",
  #root_local = "C:/kwb/projects/flextreat/hydrus/Szenarien_10day",
  #root_local =  system.file("extdata/model", package = "flextreat.hydrus1d"),
  exe_dir = "<root_local>",
  months_start = periods$start[i],
  months_end = periods$end[i],
  scenario = scenario,
  model_name = "1a2a_<scenario>_tracer_<months_start><months_end>", #"1a2a_BTA_korr_test_40d",
  model_gui_path = "<exe_dir>/<model_name>.h1d",
  modelvs_gui_path = "<exe_dir>/<model_name>_vs.h1d",
  model_dir = "<exe_dir>/<model_name>",
  model_dir_vs = "<exe_dir>/<model_name>_vs",
  scenario = "xxx",
  atmosphere = "<model_dir>/ATMOSPH.IN",
  atmosphere_vs = "<model_dir_vs>/ATMOSPH.IN",
  a_level = "<model_dir>/A_LEVEL.out",
  hydrus1d = "<model_dir>/HYDRUS1D.DAT",
  profile = "<model_dir>/PROFILE.DAT",
  obs_node = "<model_dir>/Obs_Node.out",
  balance = "<model_dir>/BALANCE.out",
  t_level = "<model_dir>/T_LEVEL.out",
  t_level_vs = "<model_dir_vs>/T_LEVEL.out",
  runinf = "<model_dir>/Run_Inf.out",
  solute_id = "1",
  solute = "<model_dir>/solute<solute_id>.out",
  solute_vs = "<model_dir_vs>/solute<solute_id>.out",
  soil_data = "<extdata>/input-data/soil/soil_geolog.csv"
)

paths <- kwb.utils::resolve(paths_list)

#fs::file_copy(paths$profile, file.path(paths$model_dir, "PROFILE_org.DAT"))

# soil_profile <- kwb.hydrus1d::read_profile(paths$profile)
#
# #soil_profile$profile <- soil_profile$profile[,1:15]
#
# solute_ids <- 6:10
#
# conc <- stats::setNames(lapply(solute_ids, function(x) rep(0, nrow(soil_profile$profile))),
#                         nm = sprintf("conc%s", solute_ids)) %>%
#   dplyr::bind_cols()
#
# profile <- dplyr::bind_cols(soil_profile$profile, conc)
#
# #profile_100 <- kwb.hydrus1d::extend_soil_profile(profile, x_end = -100)
# profile_300 <- kwb.hydrus1d::extend_soil_profile(profile, x_end = -300)
#
# #
# soil_profile_300 <- soil_profile
# soil_profile_300$profile <- profile_300
# #
# #
# kwb.hydrus1d::write_profile(soil_profile_300,
#                             path = file.path(paths$model_dir, "PROFILE.DAT"))
#
#  soil_profile <- kwb.hydrus1d::read_profile(paths$profile)
#
#  atmos <- kwb.hydrus1d::read_atmosph(paths$atmosphere)
#  sum(grepl("cTop", names(atmos$data)))
#
#  hydrus1d <- kwb.hydrus1d::read_hydrus1d(paths$hydrus1d)
#  hydrus1d$Main$NumberOfSolutes <-  sum(grepl("cTop", names(atmos$data)))
#  #sum(grepl("conc", names(soil_profile$profile)))
#  hydrus1d$Profile$ProfileDepth <- - min(soil_profile$profile$x)
#  hydrus1d$Profile$NumberOfNodes <- max(soil_profile$profile$node_id)
#  hydrus1d$Profile$ObservationNodes <- soil_profile$obsnodes$n
#
#
#  kwb.hydrus1d::write_hydrus1d(hydrus1d_list = hydrus1d,
#                               path = paths$hydrus1d)


no_irrig <- stringr::str_detect(paths$model_dir, "no-irrig")

# org <- fs::dir_ls(path =paths$exe_dir,
#            regexp = "1a2a_soil-1.5m_.*\\.h1d$")
#
# new <- stringr::str_replace(org, "1.5m", "2m")

#fs::file_copy(org, new)

# org <- fs::dir_ls(path = paths$exe_dir,
#                   regexp = "1a2a_soil-3m_.*/PROFILE.DAT",
#                   recurse = TRUE)
#
# fs::file_copy(rep(file.path(paths$exe_dir, "1a2a_soil-3m_tracer_0110/PROFILE.dat"),
#                   length(org)-1),
#               new_path = org[-1],
#               overwrite = TRUE)
#
# org <- fs::dir_ls(path = paths$exe_dir,
#                   regexp = "1a2a_soil-3m_.*/HYDRUS1D.DAT",
#                   recurse = TRUE)
#
# fs::file_copy(rep(file.path(paths$exe_dir, "1a2a_soil-3m_tracer_0110/HYDRUS1D.dat"),
#                   length(org)-1),
#               new_path = org[-1],
#               overwrite = TRUE)

fs::dir_copy(paths$model_dir, paths$model_dir_vs, overwrite = TRUE)
fs::file_copy(paths$model_gui_path, paths$modelvs_gui_path, overwrite = TRUE)
#
# profile <- kwb.hydrus1d::read_profile(paths$profile)
#
# View(profile)

library(flextreat.hydrus1d)
atm <- flextreat.hydrus1d::prepare_atmosphere_data()

#no-irrigation
if(no_irrig) atm[,c("groundwater.mmPerDay", "clearwater.mmPerDay")] <- 0

atm_selected <- flextreat.hydrus1d::select_hydrologic_years(atm)
# atm_prep <- flextreat.hydrus1d::prepare_atmosphere(atm = atm_selected,
#                                        conc_irrig_clearwater = c(6.738,
#                                                                  0.875,
#                                                                  4.291,
#                                                                  2.884,
#                                                                  1.062),
#                                        conc_irrig_groundwater = 0,
#                                        conc_rain = 0
#                                        )
atm <- atm_selected
days_monthy <- lubridate::days_in_month(seq.Date(from = min(atm$date),
                                                 to = max(atm$date),
                                                 by = "month"))

days_total <- cumsum(days_monthy)

indeces <- as.integer(paths$months_start):as.integer(paths$months_end)

c_tops <- lapply(indeces, function(i) {

  x <- rep(0, nrow(atm))
  if(i == 1) {
    x_min = 1
  } else {
    x_min = days_total[i - 1] + 1
  }
  x[x_min:days_total[i]] <- rep(100, days_monthy[i])


  tib <- data.frame(x)
  colnames(tib) <- if(i == indeces[1]) {
    "cTop"} else {
      sprintf("cTop%d", which(indeces %in% i))
    }

 tib
}) %>% dplyr::bind_cols()



if(no_irrig) {
  atm_prep <- flextreat.hydrus1d::prepare_atmosphere(atm = atm_selected,
                                                     conc_irrig_clearwater = 0,
                                                     conc_irrig_groundwater = 0,
                                                     conc_rain = c_tops
  )
} else {
  atm_prep <- flextreat.hydrus1d::prepare_atmosphere(atm = atm_selected,
                                                     conc_irrig_clearwater = c_tops,
                                                     conc_irrig_groundwater = c_tops,
                                                     conc_rain = c_tops
  )
}

writeLines(kwb.hydrus1d::write_atmosphere(atm = atm_prep),
           paths$atmosphere)

kwb.hydrus1d::run_model(model_path = paths$model_dir)

atmos <- kwb.hydrus1d::read_atmosph(paths$atmosphere)


atmos$data[names(c_tops)] <- c_tops

atm_default <- atmos

tlevel <- kwb.hydrus1d::read_tlevel(paths$t_level)

vs_atm <- flextreat.hydrus1d::recalculate_ctop_with_virtualstorage(
  atm = atm_default$data,
  tlevel = tlevel,
  crit_v_top = - 0.05
)

atmos$data[names(vs_atm$df)] <- vs_atm$df

writeLines(kwb.hydrus1d::write_atmosphere(atm = atmos$data),
           paths$atmosphere_vs)


kwb.hydrus1d::run_model(model_path = paths$model_dir_vs)

})
})

solute <- kwb.hydrus1d::read_solute(paths$solute_vs) %>%
  dplyr::mutate(difftime = c(0,diff(time)))

plot(solute$time, solute$c_top)
points(solute$c_bot, col = "red")
(1 - max(solute$sum_cv_top)/sum(atmos$data$Prec*atmos$data$cTop)) * 100


paths$solute_vs2 <- "C:/kwb/projects/flextreat/3_1_4_Prognosemodell/Vivian/Rohdaten/H1D/1a2a_tracer_vs/solute2.out"

solute <- kwb.hydrus1d::read_solute(paths$solute) %>%
  dplyr::mutate(difftime = c(0,diff(time)))

(1 - max(solute$sum_cv_top)/sum(atmos$data$Prec*atmos$data$cTop2)) * 100


sum(atmos$data$Prec)
max(tlevel$sum_infil)
max(tlevel$sum_evap)

balance <- kwb.hydrus1d::read_balance(paths$balance)

balance %>%
  dplyr::filter(subdomain_id == 0,
                time < 400,
                parameter == "CncBalT") %>%
  ggplot2::ggplot(mapping = ggplot2::aes(x = time, y = value, col = as.factor(solute_id))) + ggplot2::geom_point()



balance %>%
  dplyr::filter(subdomain_id == 0,
                time < 400,
                parameter == "CncBalR") %>%
  ggplot2::ggplot(mapping = ggplot2::aes(x = time, y = value, col = as.factor(solute_id))) +
  ggplot2::geom_point()

sum(solute$difftime * as.numeric(solute$c_top) * as.numeric(tlevel$r_top))

solute_day <- flextreat.hydrus1d::aggregate_solute(solute)


plot(solute_day$date, solute_day$c_top)
abline(h = 7)

plot(atmos$data$tAtm, atmos$data$cTop)

sum(solute_day$cv_top[solute_day$cv_top > 0])
sum(atmos$data$Prec*atmos$data$cTop)
sum(atmos$data$Prec*atmos$data$cTop)/sum(solute_day$cv_top[solute_day$cv_top > 0])
sum(solute$cv_top)

sum((as.numeric(solute$cv_top) * solute$difftime))
sum((solute$cv_ch1 * c(0,diff(solute$time))))

max(solute$sum_cv_top)

condition <- solute$cv_top > 0
sum(solute$cv_top[condition])
condition <- solute$cv_top < 0
sum(solute$cv_top[condition])

solute_aggr_date <- flextreat.hydrus1d::aggregate_solute(solute)

obsnode <- kwb.hydrus1d::read_obsnode(paths$obs_node)

obsnode %>%
  dplyr::filter(variable == "flux") %>%
  dplyr::group_by(node_id) %>%
  dplyr::summarise(sum = sum(value))

profile <- kwb.hydrus1d::read_profile(paths$profile)

p <- obsnode %>%
  dplyr::left_join(profile[,c("node_id", "x")]) %>%
  dplyr::filter(variable == "conc1") %>%
  ggplot2::ggplot(mapping = ggplot2::aes(x = time,
                                         y = value,
                                         col = as.factor(x))) +
  ggplot2::geom_point() +
  ggplot2::theme_bw()

plotly::ggplotly(p)

solute_aggr_date
View(tlevel_aggr_date)
View(solute_aggr_date)


t_level <- kwb.hydrus1d::read_tlevel(paths$t_level)
t_level

## t_level aggregate
tlevel_aggr_date <- flextreat.hydrus1d::aggregate_tlevel(t_level)
tlevel_aggr_yearmonth <- flextreat.hydrus1d::aggregate_tlevel(t_level,
                                                              col_aggr = "yearmonth")
tlevel_aggr_year_hydrologic <- flextreat.hydrus1d::aggregate_tlevel(t_level,
                                                                    col_aggr = "year_hydrologic") %>%
  dplyr::filter(.data$diff_time >= 364) ### filter out as only may-october


wb_date_plot <- flextreat.hydrus1d::plot_waterbalance(tlevel_aggr_date)
wb_yearmonth_plot <- flextreat.hydrus1d::plot_waterbalance(tlevel_aggr_yearmonth)
wb_yearhydrologic_plot <- flextreat.hydrus1d::plot_waterbalance(tlevel_aggr_year_hydrologic)

wb_date_plot
wb_yearmonth_plot
wb_yearhydrologic_plot


solute$time[min(which(solute$sum_cv_top == max(solute$sum_cv_top)))]

paths$model_dir_vs

solute_files <- fs::dir_ls(paths$exe_dir,
                           regexp = "1a2a_tracer.*_vs/solute\\d\\d?.out",
                           recurse = TRUE)



sol_travel <- lapply(solute_files, function(path) {

  solute <- kwb.hydrus1d::read_solute(path)

tibble::tibble(
  model_name = basename(dirname(path)),
  solute_name = kwb.utils::removeExtension(basename(path)),
  p01_top = mean(solute$time[which(max(solute$sum_cv_top)*0.005 <= solute$sum_cv_top &  solute$sum_cv_top <= max(solute$sum_cv_top)*0.015)], na.rm = TRUE),
  p01_bot = mean(solute$time[which(solute$sum_cv_bot >= - max(solute$sum_cv_top)*0.015  &  solute$sum_cv_bot <= - max(solute$sum_cv_top)*0.005)], na.rm = TRUE),
  p01_diff = p01_bot - p01_top,
  p05_top = mean(solute$time[which(max(solute$sum_cv_top)*0.045 <= solute$sum_cv_top &  solute$sum_cv_top <= max(solute$sum_cv_top)*0.055)], na.rm = TRUE),
  p05_bot = mean(solute$time[which(solute$sum_cv_bot >= - max(solute$sum_cv_top)*0.055 &  solute$sum_cv_bot <= - max(solute$sum_cv_top)*0.045)], na.rm = TRUE),
  p05_diff = p05_bot - p05_top,
  p10_top = mean(solute$time[which(max(solute$sum_cv_top)*0.09 <= solute$sum_cv_top &  solute$sum_cv_top <= max(solute$sum_cv_top)*0.11)], na.rm = TRUE),
  p10_bot = mean(solute$time[which(solute$sum_cv_bot >= - max(solute$sum_cv_top)*0.11 &  solute$sum_cv_bot <= - max(solute$sum_cv_top)*0.09)], na.rm = TRUE),
  p10_diff = p10_bot - p10_top,
  p25_top =  mean(solute$time[which(max(solute$sum_cv_top)*0.24 <= solute$sum_cv_top &  solute$sum_cv_top <= max(solute$sum_cv_top)*0.26)], na.rm = TRUE),
  p25_bot = mean(solute$time[which(solute$sum_cv_bot >= - max(solute$sum_cv_top)*0.26 &  solute$sum_cv_bot <= - max(solute$sum_cv_top)*0.24)], na.rm = TRUE),
  p25_diff = p25_bot - p25_top,
  p50_top = mean(solute$time[which(max(solute$sum_cv_top)*0.48 <= solute$sum_cv_top &  solute$sum_cv_top <= max(solute$sum_cv_top)*0.52)], na.rm = TRUE),
  p50_bot = mean(solute$time[which(solute$sum_cv_bot >= - max(solute$sum_cv_top)*0.520 &  solute$sum_cv_bot <= - max(solute$sum_cv_top)*0.48)], na.rm = TRUE),
  p50_diff = p50_bot - p50_top,
  p75_top =  mean(solute$time[which(max(solute$sum_cv_top)*0.725 <= solute$sum_cv_top &  solute$sum_cv_top <= max(solute$sum_cv_top)*0.775)], na.rm = TRUE),
  p75_bot = mean(solute$time[which(solute$sum_cv_bot >= - max(solute$sum_cv_top)*0.775 &  solute$sum_cv_bot <= - max(solute$sum_cv_top)*0.725)], na.rm = TRUE),
  p75_diff = p75_bot - p75_top,
  p90_top =  mean(solute$time[which(max(solute$sum_cv_top)*0.88 <= solute$sum_cv_top &  solute$sum_cv_top <= max(solute$sum_cv_top)*0.92)], na.rm = TRUE),
  p90_bot = mean(solute$time[which(solute$sum_cv_bot >= - max(solute$sum_cv_top)*0.92 &  solute$sum_cv_bot <= - max(solute$sum_cv_top)*0.88)], na.rm = TRUE),
  p90_diff = p90_bot - p90_top,
  p95_top =  mean(solute$time[which(max(solute$sum_cv_top)*0.93 <= solute$sum_cv_top &  solute$sum_cv_top <= max(solute$sum_cv_top)*0.97)], na.rm = TRUE),
  p95_bot = mean(solute$time[which(solute$sum_cv_bot >= - max(solute$sum_cv_top)*0.97 &  solute$sum_cv_bot <= - max(solute$sum_cv_top)*0.93)], na.rm = TRUE),
  p95_diff = p95_bot - p95_top,
  p99_top =  mean(solute$time[which(max(solute$sum_cv_top)*0.981 <= solute$sum_cv_top &  solute$sum_cv_top <= max(solute$sum_cv_top)*0.999)], na.rm = TRUE),
  p99_bot = mean(solute$time[which(solute$sum_cv_bot >= - max(solute$sum_cv_top)*0.999 &  solute$sum_cv_bot <= - max(solute$sum_cv_top)*0.981)], na.rm = TRUE),
  p99_diff = p99_bot - p99_top
  )
}) %>% dplyr::bind_rows()


sol_travel <- sol_travel %>%
  dplyr::mutate(solute_name = stringr::str_remove(solute_name, "solute") %>% as.integer()) %>%
  dplyr::rename(solute_id = solute_name) %>%
  dplyr::arrange(model_name, solute_id)

lookup_model <- sol_travel %>%
  dplyr::group_by(model_name) %>%
  dplyr::summarise(n = dplyr::n()) %>%
  dplyr::mutate(n_cum = cumsum(n),
                n_cum_fix = n_cum - dplyr::first(n))


library(lubridate)

get_last_day_of_months <- function(ids) {
  sapply(ids, function(id) {
    start_date <- lubridate::ymd("2017-05-01")
    month_date <- start_date %m+% months(id - 1)
    last_day <- lubridate::ceiling_date(month_date, "month") - days(1)

  return(last_day)
}) %>% as.Date()
}


sol_travel_tot <- lookup_model %>%
  dplyr::left_join(sol_travel) %>%
  dplyr::mutate(month_id = solute_id + n_cum_fix,
                date = get_last_day_of_months(month_id))


p1 <- sol_travel_tot %>%
  dplyr::select(date, tidyselect::ends_with("diff")) %>%
  tidyr::pivot_longer(- date) %>%
  dplyr::mutate(name = stringr::str_remove(name, "_diff")) %>%
  ggplot2::ggplot(ggplot2::aes(x = date, y = value, col = name)) +
  ggplot2::geom_point() +
  ggplot2::geom_line() +
  ggplot2::theme_bw()

plotly::ggplotly(p1)


traveltimes_list <- setNames(lapply(scenarios, function(scenario) {

try({

solute_files <- fs::dir_ls(paths$exe_dir,
                                   regexp = sprintf("1a2a_%s_tracer.*_vs/solute\\d\\d?.out",
                                                    scenario),
                                   recurse = TRUE)



flextreat.hydrus1d::get_traveltimes(solute_files, dbg = TRUE)
})}), nm = (scenarios))


sapply(seq_along(traveltimes_list), function(i) {

  htmlwidgets::saveWidget(flextreat.hydrus1d::plot_traveltimes(traveltimes_list[[i]],
                                                               title = sprintf("%s", names(traveltimes_list)[i]),
                                                               ylim = c(0,650)),
                          file = sprintf("traveltimes_%s.html", names(traveltimes_list)[i]))
})


traveltime_bp <- lapply(traveltimes_list, function(x) {
  x %>%
  dplyr::filter(percentiles == 0.5)
}) %>% dplyr::bind_rows(.id = "scenario") %>%
  dplyr::filter(!stringr::str_detect(scenario, "1.5"))


scenario_by_mean_traveltime <- traveltime_bp %>%
  dplyr::group_by(scenario) %>%
  dplyr::summarise(mean = mean(time_diff, na.rm = TRUE)) %>%
  dplyr::arrange(mean)


scenario_by_mean_traveltime$scenario


y_lim <- c(0,350)

tt_bp_total <- traveltime_bp %>%
  ggplot2::ggplot(ggplot2::aes(x = scenario, y = time_diff)) +
  ggplot2::geom_boxplot(outliers = FALSE) +
  ggplot2::geom_jitter(position = ggplot2::position_jitter(width = 0.1),
                       col = "darkgrey",
                       alpha = 0.6) +
  ggplot2::ylim(y_lim) +
  ggplot2::labs(y = "Median Traveltime (days)", x = "Scenario",
                title = "Boxplot: median traveltime total") +
  ggplot2::theme_bw()



tt_bp_quarter <- traveltime_bp %>%
  dplyr::mutate(quarter = lubridate::quarter(traveltime_bp$date) %>% as.factor()) %>%
ggplot2::ggplot(ggplot2::aes(x = scenario, y = time_diff, col = quarter)) +
  ggplot2::geom_boxplot() +
  ggplot2::geom_jitter(position = ggplot2::position_jitterdodge(
    jitter.width = 0.1,
    dodge.width = 0.75),
    alpha = 0.6) +
  ggplot2::labs(y = "Median Traveltime (days)",
                x = "Scenario",
                col = "Quartal",
                title = "Boxplot: median traveltime by quarter") +
  ggplot2::ylim(y_lim) +
  ggplot2::theme_bw() +
  ggplot2::theme(legend.position = "top")



htmlwidgets::saveWidget(widget = plotly::ggplotly(tt_bp_total),
                        title = "Boxplot: median traveltime total",
                        file = "boxplot_traveltimes-median_total.html")


htmlwidgets::saveWidget(plotly::ggplotly(tt_bp_quarter),
                        title = "Boxplot: median traveltime by quarter",
                        file = "boxplot_traveltimes-median_quarter.html")
