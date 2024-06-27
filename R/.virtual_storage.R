remotes::install_github("kwb-r/kwb.hydrus1d@dev")

paths_list <- list(
  #extdata = system.file("extdata", package = "flextreat.hydrus1d"),
  #root_server = "Y:/WWT_Department/Projects/FlexTreat/Work-packages/AP3/3_1_4_Prognosemodell/Vivian/Rohdaten/H1D",
  root_local = "C:/kwb/projects/flextreat/3_1_4_Prognosemodell/Vivian/Rohdaten/H1D",
  #root_local = "C:/kwb/projects/flextreat/hydrus/Szenarien_10day",
  #root_local =  system.file("extdata/model", package = "flextreat.hydrus1d"),
  exe_dir = "<root_local>",
  model_name = "test_fracht1", #"1a2a_BTA_korr_test_40d",
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

fs::dir_copy(paths$model_dir, paths$model_dir_vs, overwrite = TRUE)
fs::file_copy(paths$model_gui_path, paths$modelvs_gui_path, overwrite = TRUE)


kwb.hydrus1d::run_model(model_path = paths$model_dir)

atmos <- kwb.hydrus1d::read_atmosph(paths$atmosphere)

atm_default <- atmos

tlevel <- kwb.hydrus1d::read_tlevel(paths$t_level)

vs_atm <- flextreat.hydrus1d::recalculate_ctop_with_virtualstorage(
  atm = atm_default$data,
  tlevel = tlevel,
  crit_v_top = - 0.05
)

atmos$data$cTop <- vs_atm$cTop


writeLines(kwb.hydrus1d::write_atmosphere(atm = atmos$data),
           paths$atmosphere_vs)


kwb.hydrus1d::run_model(model_path = paths$model_dir_vs)

solute <- kwb.hydrus1d::read_solute(paths$solute) %>%
  dplyr::mutate(difftime = c(0,diff(time)))

(1 - max(solute$sum_cv_top)/sum(atmos$data$Prec*atmos$data$cTop)) * 100

solute <- kwb.hydrus1d::read_solute(paths$solute_vs) %>%
  dplyr::mutate(difftime = c(0,diff(time)))

(1 - max(solute$sum_cv_top)/sum(atmos$data$Prec*atmos$data$cTop)) * 100


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

ssolute_aggr_date
View(tlevel_aggr_date)
View(solute_aggr_date)
