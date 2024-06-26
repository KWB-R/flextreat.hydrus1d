remotes::install_github("kwb-r/kwb.hydrus1d@dev")

paths_list <- list(
  #extdata = system.file("extdata", package = "flextreat.hydrus1d"),
  #root_server = "Y:/WWT_Department/Projects/FlexTreat/Work-packages/AP3/3_1_4_Prognosemodell/Vivian/Rohdaten/H1D",
  root_local = "C:/kwb/projects/flextreat/3_1_4_Prognosemodell/Vivian/Rohdaten/H1D",
  #root_local = "C:/kwb/projects/flextreat/hydrus/Szenarien_10day",
  #root_local =  system.file("extdata/model", package = "flextreat.hydrus1d"),
  exe_dir = "<root_local>",
  model_name = "test_fracht1_org", #"1a2a_BTA_korr_test_40d",
  model_dir = "<exe_dir>/<model_name>",
  scenario = "xxx",
  atmosphere = "<model_dir>/ATMOSPH.IN",
  a_level = "<model_dir>/A_LEVEL.out",
  profile = "<model_dir>/PROFILE.dat",
  obs_node = "<model_dir>/Obs_Node.out",
  balance = "<model_dir>/BALANCE.out",
  t_level = "<model_dir>/T_LEVEL.out",
  runinf = "<model_dir>/Run_Inf.out",
  solute_id = "1",
  solute = "<model_dir>/solute<solute_id>.out",
  soil_data = "<extdata>/input-data/soil/soil_geolog.csv"
)


paths <- kwb.utils::resolve(paths_list)


atmos <- kwb.hydrus1d::read_atmosph(paths$atmosphere)


#atmos$data$cTop <- atmos$data$cTop*1000
evap <- atmos$data$rSoil
atmos$data$rSoil <- tlevel_aggr_date$evap[-1281]


condition <- atmos$data$Prec - atmos$data$rSoil < 0


atm_default <- atmos

#result2$tAtm_start


#atmos$data$cTop[result2$tAtm_start] <- dplyr::coalesce(result2$cor_Prec_cTop_first, 0)

#atmos$data$cTop <- result3$cTop
atmos$data$cTop <- vs_atm$cTop

#atmos$data$rSoil[condition]  <- dplyr::if_else(atmos$data$Prec[condition] > 0.1,
#                                               atmos$data$Prec[condition] - 0.1,
#                                               0)

writeLines(kwb.hydrus1d::write_atmosphere(atm = atmos$data),
           paths$atmosphere)



View(atmos$data)

kwb.hydrus1d::run_model(model_path = paths$model_dir)

solute <- kwb.hydrus1d::read_solute(paths$solute) %>%
  dplyr::mutate(difftime = c(0,diff(time)))

(1 - max(solute$sum_cv_top)/sum(atmos$data$Prec*atmos$data$cTop)) * 100

tlevel <- kwb.hydrus1d::read_tlevel(paths$t_level)

vs_atm <- flextreat.hydrus1d::recalculate_ctop_with_virtualstorage(
  atm = atm_default$data,
  tlevel = tlevel,
  crit_v_top = - 0.05
  )


which(atmos$data$Prec - tlevel_aggr_date$evap[-1281] < 0)

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

obsnode %>%
  dplyr::left_join(profile[,c("node_id", "x")]) %>%
  dplyr::filter(variable == "conc1") %>%
  ggplot2::ggplot(mapping = ggplot2::aes(x = time, y = value, col = as.factor(x))) +
  ggplot2::geom_point() +
  ggplot2::theme_bw()

solute_aggr_date
View(tlevel_aggr_date)
View(solute_aggr_date)

recalculate_ctop_with_virtualstorage(atm = atmos$data)

