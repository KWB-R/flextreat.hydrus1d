remotes::install_github("kwb-r/kwb.hydrus1d@dev")
remotes::install_github("kwb-r/flextreat.hydrus1d@dev")
library(magrittr)
library(flextreat.hydrus1d)

get_atm <- function(atm, extreme_rain = NULL) {

  if(is.null(extreme_rain)) {
    return(atm)
  } else if (extreme_rain %in% c("dry", "wet")) {
    atm <- atm %>%
      dplyr::mutate(hydrologic_year = flextreat.hydrus1d::get_hydrologic_years(date),
                    year = as.integer(format(date, format = "%Y")),
                    day_of_year = lubridate::yday(date))

    atm_stats <- atm %>%
      dplyr::group_by(year) %>%
      dplyr::summarise(rain_mm = sum(rain_mm, na.rm = TRUE),
                       evapo_p_mean_mm = sum(evapo_p_mean_mm, na.rm = TRUE))

    if(extreme_rain == "dry") {
      atm_dry <- atm_stats %>%
        dplyr::filter(rain_mm == min(rain_mm))

      atm_dry_sel <- atm %>%
        dplyr::filter(year == atm_dry$year) %>%
        dplyr::select(day_of_year, rain_mm)

      atm_dry_input <- atm %>%
        dplyr::select(- rain_mm, - hydrologic_year, - year) %>%
        dplyr::left_join(atm_dry_sel) %>%
        dplyr::mutate(rain_mm = dplyr::if_else(is.na(rain_mm), 0, rain_mm)) %>%
        dplyr::select(- day_of_year) %>%
        dplyr::relocate(rain_mm, .after = clearwater.mmPerDay)

      return(atm_dry_input)
    } else if (extreme_rain == "wet") {
      atm_wet <- atm_stats %>%
        dplyr::filter(rain_mm == max(rain_mm))


      atm_wet_sel <- atm %>%
        dplyr::filter(year == atm_wet$year) %>%
        dplyr::select(day_of_year, rain_mm)

      atm_wet_input <- atm %>%
        dplyr::select(- rain_mm, - hydrologic_year, - year) %>%
        dplyr::left_join(atm_wet_sel) %>%
        dplyr::mutate(rain_mm = dplyr::if_else(is.na(rain_mm), 0, rain_mm)) %>%
        dplyr::select(- day_of_year) %>%
        dplyr::relocate(rain_mm, .after = clearwater.mmPerDay)
      return(atm_wet_input)
    } else {
      stop("extreme_rain has to be either 'NULL', 'dry' or 'wet")
    }
  }}


prepare_solute_input <- function(dat,
                                 selector,
                                 Ks = NULL,
                                 SnkL1 = NULL,
                                 diff_w = 0, diff_g = 0) {

  stopifnot(nrow(dat) <= 10)

  if(selector$solute$No.Solutes != nrow(dat)) {
    selector$solute$No.Solutes <- nrow(dat)
  }

  solute_names <- sprintf("solute_%d", seq_len(nrow(dat)))

  solutes_new <- setNames(lapply(seq_len(nrow(dat)), function(i) {
    dat_sel <- dat[i,]

    ks <- kd(porosity = selector$waterflow$soil$ths - selector$waterflow$soil$thr,
             retardation = dat_sel$retard,
             bulk_density = selector$solute$transport$Bulk.d.)

    cols <- c("Ks", "Nu", "Beta", "Henry", "SnkL1", "SnkS1", "SnkG1", "SnkL1'",
              "SnkS1'", "SnkG1'", "SnkL0",  "SnkS0", "SnkG0",  "Alfa")

    reaction <- matrix(data = 0, ncol = length(cols), nrow = length(ks)) %>%
      as.data.frame() %>% tibble::as_tibble()
    names(reaction) <- cols
    reaction$Beta <- 1
    reaction$Ks <- if(is.null(Ks)) {round(ks, 2)} else { Ks}
    reaction$SnkL1 <- if(is.null(SnkL1)) {
      round(halftime_to_firstorderrate(dat_sel$half_life_days), 5)
    } else {
      SnkL1
      }

    list(diffusion = tibble::tibble(DifW = diff_w, DifG = diff_g),
         reaction = reaction)
  }), nm = solute_names)

  sel_tmp <- selector$solute[!names(selector$solute) %in% solute_names]

  solutes_new_list <- c(sel_tmp[1:which(names(sel_tmp) == "transport")],
                        solutes_new,
                        sel_tmp[which(names(sel_tmp) == "kTopSolute"):length(sel_tmp)])

  c(selector[names(selector) != "solute"],
    list(solute = solutes_new_list))
}

get_mean <- function(col) {
  x <- col %>% stringr::str_split_fixed("-", n = 2)
  mode(x) <- "numeric"
  round(rowMeans(x), digits = 2)
}


kd <- function(porosity, retardation, bulk_density) {

  #https://www3.epa.gov/ceampubl/learn2model/part-two/onsite/retard.html
  (retardation - 1) * porosity / bulk_density

}

halftime_to_firstorderrate <- function(half_time) {

  if(half_time != 0) {
  #https://chem.libretexts.org/Courses/Bellarmine_University/BU%3A_Chem_104_(Christianson)/Phase_2%3A_Understanding_Chemical_Reactions/4%3A_Kinetics%3A_How_Fast_Reactions_Go/4.5%3A_First_Order_Reaction_Half-Life#mjx-eqn-21.4.2
  0.693 / half_time
  } else {
    0
  }
}

#path <- "Y:/WWT_Department/Projects/FlexTreat/Work-packages/AP3/3_1_2_Boden-Grundwasser/daten_karten/Sickerwasserprognose/column-studies/Stoffeigenschaften_Säulen.xlsx"
path <- "Y:/WWT_Department/Projects/FlexTreat/Work-packages/AP3/3_1_4_Prognosemodell/StofflicheModellrandbedingungen.xlsx"

soil_columns <- kwb.db::hsGetTable(path, "my_results2", stringsAsFactors = FALSE) %>%
  janitor::clean_names() %>%
  dplyr::mutate(half_life_days = dplyr::case_when(
    grepl(">", hwz_tage) ~ hwz_tage %>% stringr::str_remove(">") %>% as.numeric() %>% round(digits = 2),
    grepl("<", hwz_tage) ~ hwz_tage %>% stringr::str_remove("<") %>% as.numeric() %>% round(digits = 2),
    grepl("-", hwz_tage) ~ get_mean(hwz_tage),
    .default = as.numeric(hwz_tage) %>% round(digits = 2)),
    retard = dplyr::case_when(
      grepl("-", retardation) ~ get_mean(retardation),
      is.na(retardation) ~ 1L,
      .default = as.numeric(retardation) %>% round(digits = 2))) %>%
  dplyr::filter(!is.na(half_life_days)) %>%
  dplyr::mutate(id = 1:dplyr::n()) %>%
  dplyr::relocate(id) #%>%
  #dplyr::mutate(retard = 1#,
                #half_life_days = 0
  #              )


### Select 1 substance for 5 different half life classes defined in this table
# selected_substances <- readr::read_csv("inst/extdata/input-data/substance_classes.csv")
#
# soil_columns <- soil_columns %>%
#   dplyr::filter(substanz_nr %in% selected_substances$substance_id) %>%
#   dplyr::mutate(id = 1:dplyr::n())


tracer <- TRUE
short <- FALSE

irrig_only_growing_season <- FALSE
irrig_dir_string <- if(irrig_only_growing_season) {
  "irrig-period_growing-season"
} else {
  "irrig-period_status-quo"
}

duration_string <- if (short == FALSE) {
  "long"
} else {
  "short"
}

extreme_rain <- NULL #NULL # "wet", "dry"


extreme_rain_string <- if(any(c("dry", "wet") %in% extreme_rain)) {
  sprintf("_%s", extreme_rain)
} else {
  ""
}

scenarios <- sapply(c(1,10), function(x) paste0("soil-", 1:3, sprintf("m_irrig-%02ddays", x))) %>%
  as.vector()
#scenarios <- scenarios[c(1,3)]
seq_start <- seq(1,nrow(soil_columns),10)

seq_end <- if(nrow(soil_columns) > 10) {
  seq(10,nrow(soil_columns),10)
} else {
  nrow(soil_columns)
}


if(length(seq_end) < length(seq_start)) seq_end <- c(seq_end, nrow(soil_columns))

solute_ids <- tibble::tibble(start = as.integer(seq_start),
                              end = as.integer(seq_end))
treatments <- "tracer" #c("ka", "o3") #"ka" #c("ka", "o3") #"ka"
#treatments <- c("ka")

atm <- get_atm(atm = flextreat.hydrus1d::prepare_atmosphere_data(),
               extreme_rain = extreme_rain)

if(irrig_only_growing_season) {
  atm[which(!lubridate::month(atm$date) %in% 4:9), c("groundwater.mmPerDay", "clearwater.mmPerDay")] <- 0
}


atm <- if(short) {
  atm %>%
    dplyr::filter(date >= "2017-05-01" & date <= "2020-04-30")
} else {
  atm %>%
    dplyr::filter(date >= "2017-05-01" & date <= "2023-12-31")
}

days_monthy <- lubridate::days_in_month(seq.Date(from = min(atm$date),
                                                 to = max(atm$date),
                                                 by = "month"))

periods <- tibble::tibble(start = seq(1,length(days_monthy),10),
                          end = if(length(days_monthy) %% 10 != 0) {
                            c(seq(10,length(days_monthy),10), length(days_monthy))
                            } else {seq(10,length(days_monthy),10)
                            }
                          )

sapply(treatments, function(treatment) {
sapply(scenarios, function(scenario) {

tracer <- if(treatment == "tracer") { TRUE } else { FALSE}

loop_df <- if(tracer) {
  periods
} else {
  solute_ids
}

sapply(seq_len(nrow(loop_df)), function(i) {

  paths_list <- list(
    #extdata = system.file("extdata", package = "flextreat.hydrus1d"),
    #root_server = "Y:/WWT_Department/Projects/FlexTreat/Work-packages/AP3/3_1_4_Prognosemodell/Vivian/Rohdaten/retardation_no",
    root_local = sprintf("C:/kwb/projects/flextreat/3_1_4_Prognosemodell/Vivian/Rohdaten/irrig_fixed/%s/%s/tracer", irrig_dir_string, sprintf("%s%s", duration_string, extreme_rain_string)),
    #root_local = sprintf("D:/hydrus1d/irrig_fixed/%s/%s/retardation_yes", irrig_dir_string, sprintf("%s%s", duration_string, extreme_rain_string)),
    #root_local = "C:/kwb/projects/flextreat/hydrus/Szenarien_10day",
    #root_local =  system.file("extdata/model", package = "flextreat.hydrus1d"),
    exe_dir = "<root_local>",
    solute_id_start = sprintf("%02d", loop_df$start[i]),
    solute_id_end = sprintf("%02d", loop_df$end[i]),
    # months_start = periods$start[i],
    # months_end = periods$end[i],
    scenario = scenario,
    location = if(tracer) {"tracer"} else {sprintf("ablauf_%s_median", treatment)}, #"ablauf_ka_median",
    model_name_org = "model_to_copy",
    model_name = "<location>_<scenario>_soil-column_<solute_id_start><solute_id_end>",
    model_gui_path_org =  "<exe_dir>/<model_name_org>.h1d",
    model_gui_path = "<exe_dir>/<model_name>.h1d",
    modelvs_gui_path = "<exe_dir>/<model_name>_vs.h1d",
    model_dir_org = "<exe_dir>/<model_name_org>",
    model_dir = "<exe_dir>/<model_name>",
    model_dir_vs = "<exe_dir>/<model_name>_vs",
    atmosphere = "<model_dir>/ATMOSPH.IN",
    atmosphere_vs = "<model_dir_vs>/ATMOSPH.IN",
    a_level = "<model_dir>/A_LEVEL.out",
    hydrus1d = "<model_dir>/HYDRUS1D.DAT",
    selector = "<model_dir>/SELECTOR.IN",
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

  solute_start_id <- as.numeric(paths$solute_id_start)
  solute_end_id <- as.numeric(paths$solute_id_end)
  n_solutes <- solute_end_id - (solute_start_id - 1)


no_irrig <- stringr::str_detect(paths$model_dir, "no-irrig")
irrig_pattern <- "irrig-[0-9][0-9]?days"
irrig_int <- stringr::str_detect(paths$model_dir, irrig_pattern)

if(irrig_int) {

string_irrig <- stringr::str_extract(paths$model_dir, sprintf("_%s", irrig_pattern))

if(!fs::dir_exists(paths$model_dir)) {

fs::dir_copy(paths$model_dir_org, paths$model_dir)
}

if(!fs::file_exists(paths$model_gui_path)) {
  fs::file_copy(paths$model_gui_path_org, paths$model_gui_path)

}

model_out_files <- list.files(paths$model_dir, pattern = "\\.out$", full.names = TRUE)

if(length(model_out_files) > 0) {
fs::file_delete(model_out_files)
}

string_irrig_int <- stringr::str_extract(paths$model_dir, "[0-9][0-9]?days")

irrig_interval <- sprintf("%s %s",
        string_irrig_int %>%
          stringr::str_extract("\\d+") %>%
          as.integer(),
        string_irrig_int %>%
          stringr::str_extract("[a-z]+"))
}


library(flextreat.hydrus1d)

#no-irrigation
if(no_irrig) atm[,c("groundwater.mmPerDay", "clearwater.mmPerDay")] <- 0


sum_per_interval <- function(data, interval) {

  data_org <- data

  data <- data %>%
    dplyr::select(tidyselect::all_of(c("date",
                                       "groundwater.mmPerDay",
                                       "clearwater.mmPerDay")))

  cols_sum <- names(data)[names(data) != "date"]

  data_summed <- data %>%
    dplyr::mutate(group = lubridate::floor_date(date, unit = interval)) %>%  # Konvertiere date in datetime-Format
    dplyr::group_by(group) %>%  # Gruppiere nach Zeitintervallen
    dplyr::summarise_at(.vars = tidyselect::all_of(cols_sum),
                        .funs = sum) %>%   # Berechne die Summe für jedes Intervall
    dplyr::rename(date = group)

  data_org[, cols_sum] <- 0
  data_org[data_org$date %in% data_summed$date, cols_sum] <- data_summed[,cols_sum]
  data_org
}

if(irrig_int) {

atm <- sum_per_interval(data = atm, interval = irrig_interval)

}

days_total <- cumsum(days_monthy)

indeces <- as.integer(1:length(days_total))

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

c_tops_sel <- c_tops[,paths$solute_id_start:paths$solute_id_end]


atm_prep <- if(tracer) {
  flextreat.hydrus1d::prepare_atmosphere(atm = atm,
                                         conc_irrig_clearwater = c_tops_sel,
                                         conc_irrig_groundwater = c_tops_sel,
                                         conc_rain = c_tops_sel)
} else {
  flextreat.hydrus1d::prepare_atmosphere(atm = atm,
                                       conc_irrig_clearwater = soil_columns[solute_start_id:solute_end_id, paths$location],
                                       conc_irrig_groundwater = 0,
                                       conc_rain = 0
                                       )
}

n_tsteps <- nrow(atm_prep)

writeLines(kwb.hydrus1d::write_atmosphere(atm = atm_prep),
           paths$atmosphere)


selector <- kwb.hydrus1d::read_selector(path = paths$selector)

selector$time$tMax <- n_tsteps
selector$time$MPL <- 250

selector$time$TPrint <- seq(0, n_tsteps, n_tsteps/selector$time$MPL)

if (selector$time$TPrint[1] == 0) {
  selector$time$TPrint[1] <- 1
}

solutes_parameters <- if(tracer) {

} else {
  soil_columns[solute_start_id:solute_end_id,]
}

solutes_new <- prepare_solute_input(dat = soil_columns[solute_start_id:solute_end_id,],
                                    Ks = if(tracer) {0} else {NULL},
                                    SnkL1 = if(tracer) {0} else {NULL},
                                    selector = selector,
                                    diff_w = 0,
                                    diff_g = 0)


kwb.hydrus1d::write_selector(solutes_new, paths$selector)



hydrus1d <- kwb.hydrus1d::read_hydrus1d(paths$hydrus1d)
hydrus1d$Main$NumberOfSolutes <- n_solutes
kwb.hydrus1d::write_hydrus1d(hydrus1d, paths$hydrus1d)

kwb.hydrus1d::run_model(model_path = paths$model_dir)

atmos <- kwb.hydrus1d::read_atmosph(paths$atmosphere)

#atmos$data[names(c_tops)] <- c_tops

atm_default <- atmos

tlevel <- kwb.hydrus1d::read_tlevel(paths$t_level)

vs_atm <- flextreat.hydrus1d::recalculate_ctop_with_virtualstorage(
  atm = atm_default$data,
  tlevel = tlevel,
  crit_v_top = - 0.05
)

atmos$data[names(vs_atm$df)] <- vs_atm$df

fs::dir_copy(paths$model_dir, paths$model_dir_vs, overwrite = TRUE)
fs::file_copy(paths$model_gui_path, paths$modelvs_gui_path, overwrite = TRUE)

model_vs_out_files <- list.files(paths$model_dir_vs, pattern = "\\.out$", full.names = TRUE)

if(length(model_vs_out_files) > 0) {
  fs::file_delete(model_vs_out_files)
}

writeLines(kwb.hydrus1d::write_atmosphere(atm = atmos$data),
           paths$atmosphere_vs)

kwb.hydrus1d::run_model(model_path = paths$model_dir_vs)

})
})
})





solute <- kwb.hydrus1d::read_solute(paths$solute_vs) %>%
  dplyr::mutate(difftime = c(0,diff(time)))

max(solute$sum_cv_top) + min(solute$sum_cv_bot) + min(solute$cv_ch1)

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

#scenarios_solutes <- paste0(scenarios, "_soil-column")
scenarios_solutes <- paste0("ablauf_", c("o3", "ka"), "_median")


scenario_dirs <- fs::dir_ls(path = "C:/kwb/projects/flextreat/3_1_4_Prognosemodell/Vivian/Rohdaten/irrig_fixed", recurse = TRUE,  regexp = "retardation.*vs$", type = "directory")


sapply(scenario_dirs, function(scenario_dir) {

solutes_list <- setNames(lapply(scenarios_solutes, function(scenario) {
  solute_files <- fs::dir_ls(scenario_dir,
                           regexp = "solute\\d\\d?.out",
                           recurse = TRUE)

stopifnot(length(solute_files) > 0)

solutes <- setNames(lapply(solute_files, function(file) {
kwb.hydrus1d::read_solute(file, dbg = TRUE)
}), nm = solute_files) %>% dplyr::bind_rows(.id = "path")


solute_files_df <- tibble::tibble(path = solute_files,
                                  model_solute_id = path  %>%  basename() %>% stringr::str_extract(pattern = "[0-9][0-9]?") %>% as.integer(),
                                  soilcolumn_id_start = path  %>%  dirname() %>% stringr::str_extract(pattern = "[0-9]{4}") %>% stringr::str_sub(1,2) %>% as.integer(),
                                  soilcolumn_id_end = path  %>%  dirname() %>% stringr::str_extract(pattern = "[0-9]{4}") %>% stringr::str_sub(3,4) %>% as.integer(),
                                  soil_column_id = soilcolumn_id_start + model_solute_id - 1) %>%
  dplyr::left_join(soil_columns, by = c(soil_column_id = "id"))


dplyr::left_join(solutes, solute_files_df)
}), nm = scenarios_solutes)

solutes_df <- solutes_list %>%
  dplyr::bind_rows(.id = "scenario")

solutes_df_stats <- solutes_df %>%
  dplyr::bind_rows(.id = "scenario") %>%
  dplyr::mutate(scen = stringr::str_remove(basename(dirname(path)), "_soil-column.*")) %>%
  dplyr::group_by(path, scen,substanz_nr, substanz_name) %>%
  dplyr::summarise(sum_cv_top = max(sum_cv_top),
                   sum_cv_bot = min(sum_cv_bot),
                   cv_ch1 = min(cv_ch1)) %>%
  dplyr::mutate(mass_balance_error_percent = 100*(sum_cv_top + cv_ch1 + sum_cv_bot)/sum_cv_top) %>%
  dplyr::arrange(mass_balance_error_percent)


solutes_df_stats$soil <- solutes_df_stats$sum_cv_top + solutes_df_stats$sum_cv_bot + solutes_df_stats$cv_ch1

saveRDS(solutes_df, file = file.path(scenario_dir, "solutes.rds"))

openxlsx::write.xlsx(solutes_df_stats, file = file.path(scenario_dir, "hydrus_scenarios.xlsx"))
})


scenario_dirs <- fs::dir_ls(path = "C:/kwb/projects/flextreat/3_1_4_Prognosemodell/Vivian/Rohdaten/irrig_fixed/", recurse = TRUE,  regexp = "retardation_no/hydrus_scenarios.xlsx$", type = "file")

res_stats <- stats::setNames(lapply(scenario_dirs, function(scenario_dir) {
  readxl::read_excel(scenario_dir)
}), nm = scenario_dirs)

View(solutes_list$`soil-1m_irrig-01days_soil-column`)


traveltimes_list <- setNames(lapply(scenarios, function(scenario) {

  try({

    solute_files <- fs::dir_ls(paths$exe_dir,
                               regexp = sprintf("tracer.*_vs/solute\\d\\d?.out",
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
  dplyr::filter(!stringr::str_detect(scenario, "1.5")) %>%
  dplyr::mutate(quarter = lubridate::quarter(traveltime_bp$date) %>% as.factor(),
                soil_depth =  stringr::str_extract(scenario, "soil-.*m") %>%
                  stringr::str_remove_all("soil-|m") %>%  as.factor())


scenario_by_median_traveltime <- traveltime_bp %>%
  dplyr::group_by(scenario) %>%
  dplyr::summarise(median = median(time_diff, na.rm = TRUE)) %>%
  dplyr::arrange(median)

traveltime_bp <- traveltime_bp %>%
  dplyr::left_join(scenario_by_median_traveltime)

scenario_by_mean_traveltime$scenario


y_lim <- c(0,350)


tt_bp_total <- traveltime_bp %>%
  ggplot2::ggplot(ggplot2::aes(x = forcats::fct_reorder(scenario, median), y = time_diff)) +
  ggplot2::geom_boxplot(outliers = FALSE) +
  ggplot2::geom_jitter(position = ggplot2::position_jitter(width = 0.1),
                       col = "darkgrey",
                       alpha = 0.6) +
  ggplot2::ylim(y_lim) +
  ggplot2::labs(y = "Median Traveltime (days)", x = "Scenario",
                title = "Boxplot: median traveltime total") +
  ggplot2::theme_bw()

tt_bp_total


tt_bp_total_soil <- traveltime_bp %>%
  ggplot2::ggplot(ggplot2::aes(x = forcats::fct_reorder(scenario, median), y = time_diff, col = soil_depth)) +
  ggplot2::geom_boxplot(outliers = FALSE) +
  ggplot2::geom_jitter(position = ggplot2::position_jitter(width = 0.1),
                       alpha = 0.6) +
  ggplot2::ylim(y_lim) +
  ggplot2::labs(y = "Median Traveltime (days)", x = "Scenario",
                col = "Soil Depth (m)",
                title = "Boxplot: median traveltime total") +
  ggplot2::theme_bw() +
  ggplot2::theme(legend.position = "top")

tt_bp_total_soil



tt_bp_total_quartal <- traveltime_bp %>%
  ggplot2::ggplot(ggplot2::aes(x = forcats::fct_reorder(scenario, median), y = time_diff)) +
  ggplot2::geom_boxplot(outliers = FALSE) +
  ggplot2::geom_jitter(position = ggplot2::position_jitter(width = 0.1),
                       mapping = ggplot2::aes(col = quarter),
                       alpha = 0.6) +
  ggplot2::ylim(y_lim) +
  ggplot2::labs(y = "Median Traveltime (days)", x = "Scenario",
                col = "Quartal",
                title = "Boxplot: median traveltime total") +
  ggplot2::theme_bw() +
  ggplot2::theme(legend.position = "top")

tt_bp_total_quartal



tt_bp_quarter <- traveltime_bp %>%
  ggplot2::ggplot(ggplot2::aes(x = forcats::fct_reorder(scenario, median), y = time_diff, col = quarter)) +
  ggplot2::geom_boxplot(outliers = FALSE) +
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

tt_bp_quarter

htmlwidgets::saveWidget(widget = plotly::ggplotly(tt_bp_total),
                        title = "Boxplot: median traveltime total",
                        file = "boxplot_traveltimes-median_total.html")


htmlwidgets::saveWidget(plotly::ggplotly(tt_bp_quarter),
                        title = "Boxplot: median traveltime by quarter",
                        file = "boxplot_traveltimes-median_quarter.html")

