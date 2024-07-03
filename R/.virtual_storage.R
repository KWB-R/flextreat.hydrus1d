remotes::install_github("kwb-r/kwb.hydrus1d@dev")
remotes::install_github("kwb-r/flextreat.hydrus1d@dev")

paths_list <- list(
  #extdata = system.file("extdata", package = "flextreat.hydrus1d"),
  #root_server = "Y:/WWT_Department/Projects/FlexTreat/Work-packages/AP3/3_1_4_Prognosemodell/Vivian/Rohdaten/H1D",
  root_local = "C:/kwb/projects/flextreat/3_1_4_Prognosemodell/Vivian/Rohdaten/H1D",
  #root_local = "C:/kwb/projects/flextreat/hydrus/Szenarien_10day",
  #root_local =  system.file("extdata/model", package = "flextreat.hydrus1d"),
  exe_dir = "<root_local>",
  model_name = "1a2a_tracer_3140", #"1a2a_BTA_korr_test_40d",
  model_gui_path = "<exe_dir>/<model_name>.h1d",
  modelvs_gui_path = "<exe_dir>/<model_name>_vs.h1d",
  model_dir = "<exe_dir>/<model_name>",
  model_dir_vs = "<exe_dir>/<model_name>_vs",
  scenario = "xxx",
  atmosphere = "<model_dir>/ATMOSPH.IN",
  atmosphere_vs = "<model_dir_vs>/ATMOSPH.IN",
  a_level = "<model_dir>/A_LEVEL.out",
  profile = "<model_dir>/PROFILE.dat",
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
paths$solute

fs::dir_copy(paths$model_dir, paths$model_dir_vs, overwrite = TRUE)
fs::file_copy(paths$model_gui_path, paths$modelvs_gui_path, overwrite = TRUE)


library(flextreat.hydrus1d)
atm <- flextreat.hydrus1d::prepare_atmosphere_data()
atm_selected <- flextreat.hydrus1d::select_hydrologic_years(atm)
# atm_prep <- flextreat.hydrus1d::prepare_atmosphere(atm = atm_selected,
#                                        conc_irrig_clearwater = c(6738,
#                                                                  875,
#                                                                  4291,
#                                                                  2884,
#                                                                  1062),
#                                        conc_irrig_groundwater = 0,
#                                        conc_rain = 0
#                                        )
atm <- atm_selected
days_monthy <- lubridate::days_in_month(seq.Date(from = min(atm$date),
                                                 to = max(atm$date),
                                                 by = "month"))

days_total <- cumsum(days_monthy)

indeces <- 31:40

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




atm_prep <- flextreat.hydrus1d::prepare_atmosphere(atm = atm_selected,
                                       conc_irrig_clearwater = c_tops,
                                       conc_irrig_groundwater = 0,
                                       conc_rain = 0
                                       )


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
  p01_bot = mean(solute$time[which(- max(solute$sum_cv_top)*0.015 >= solute$sum_cv_bot &  solute$sum_cv_bot <= - max(solute$sum_cv_top)*0.005)], na.rm = TRUE),
  p01_diff = p01_bot - p01_top,
  p05_top = mean(solute$time[which(max(solute$sum_cv_top)*0.045 <= solute$sum_cv_top &  solute$sum_cv_top <= max(solute$sum_cv_top)*0.055)], na.rm = TRUE),
  p05_bot = mean(solute$time[which(- max(solute$sum_cv_top)*0.055 >= solute$sum_cv_bot &  solute$sum_cv_bot <= - max(solute$sum_cv_top)*0.045)], na.rm = TRUE),
  p05_diff = p05_bot - p05_top,
  p10_top = mean(solute$time[which(max(solute$sum_cv_top)*0.09 <= solute$sum_cv_top &  solute$sum_cv_top <= max(solute$sum_cv_top)*0.11)], na.rm = TRUE),
  p10_bot = mean(solute$time[which(- max(solute$sum_cv_top)*0.11 >= solute$sum_cv_bot &  solute$sum_cv_bot <= - max(solute$sum_cv_top)*0.09)], na.rm = TRUE),
  p10_diff = p10_bot - p10_top,
  p25_top =  mean(solute$time[which(max(solute$sum_cv_top)*0.24 <= solute$sum_cv_top &  solute$sum_cv_top <= max(solute$sum_cv_top)*0.26)], na.rm = TRUE),
  p25_bot = mean(solute$time[which(- max(solute$sum_cv_top)*0.26 >= solute$sum_cv_bot &  solute$sum_cv_bot <= - max(solute$sum_cv_top)*0.24)], na.rm = TRUE),
  p25_diff = p25_bot - p25_top,
  p50_top = mean(solute$time[which(max(solute$sum_cv_top)*0.48 <= solute$sum_cv_top &  solute$sum_cv_top <= max(solute$sum_cv_top)*0.52)], na.rm = TRUE),
  p50_bot = mean(solute$time[which(- max(solute$sum_cv_top)*0.52 >= solute$sum_cv_bot &  solute$sum_cv_bot <= - max(solute$sum_cv_top)*0.48)], na.rm = TRUE),
  p50_diff = p50_bot - p50_top,
  p75_top =  mean(solute$time[which(max(solute$sum_cv_top)*0.73 <= solute$sum_cv_top &  solute$sum_cv_top <= max(solute$sum_cv_top)*0.77)], na.rm = TRUE),
  p75_bot =mean(solute$time[which(- max(solute$sum_cv_top)*0.77 >= solute$sum_cv_bot &  solute$sum_cv_bot <= - max(solute$sum_cv_top)*0.73)], na.rm = TRUE),
  p75_diff = p75_bot - p75_top,
  p90_top =  mean(solute$time[which(max(solute$sum_cv_top)*0.88 <= solute$sum_cv_top &  solute$sum_cv_top <= max(solute$sum_cv_top)*0.92)], na.rm = TRUE),
  p90_bot = mean(solute$time[which(- max(solute$sum_cv_top)*0.92 >= solute$sum_cv_bot &  solute$sum_cv_bot <= - max(solute$sum_cv_top)*0.88)], na.rm = TRUE),
  p90_diff = p90_bot - p90_top,
  p95_top =  mean(solute$time[which(max(solute$sum_cv_top)*0.935 <= solute$sum_cv_top &  solute$sum_cv_top <= max(solute$sum_cv_top)*0.965)], na.rm = TRUE),
  p95_bot = mean(solute$time[which(- max(solute$sum_cv_top)*0.965 >= solute$sum_cv_bot &  solute$sum_cv_bot <= - max(solute$sum_cv_top)*0.935)], na.rm = TRUE),
  p95_diff = p95_bot - p95_top,
  p99_top =  mean(solute$time[which(max(solute$sum_cv_top)*0.98 <= solute$sum_cv_top &  solute$sum_cv_top <= max(solute$sum_cv_top)*1)], na.rm = TRUE),
  p99_bot = mean(solute$time[which(- max(solute$sum_cv_top)*1 >= solute$sum_cv_bot &  solute$sum_cv_bot <= - max(solute$sum_cv_top)*0.98)], na.rm = TRUE),
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
