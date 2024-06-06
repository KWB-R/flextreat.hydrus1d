remotes::install_github("kwb-r/kwb.hydrus1d@dev")

paths_list <- list(
  #extdata = system.file("extdata", package = "flextreat.hydrus1d"),
  #root_server = "Y:/WWT_Department/Projects/FlexTreat/Work-packages/AP3/3_1_4_Prognosemodell/Vivian/Rohdaten/H1D",
  #root_local = "C:/kwb/projects/flextreat/3_1_4_Prognosemodell/Vivian/Rohdaten/H1D",
  root_local = "C:/kwb/projects/flextreat/hydrus/Szenarien_10day",
  exe_dir = "<root_local>",
  model_name = "1a2a - Kopie",
  model_dir = "<exe_dir>/<model_name>",
  scenario = "1a2a",
  atmosphere = "<model_dir>/ATMOSPH.IN",
  a_level = "<model_dir>/A_LEVEL.out",
  profile = "<model_dir>/PROFILE.dat",
  obs_node = "<model_dir>/Obs_Node.out",
  t_level = "<model_dir>/T_LEVEL.out",
  runinf = "<model_dir>/Run_Inf.out",
  solute_id = "1",
  solute = "<model_dir>/solute<solute_id>.out",
  soil_data = "<extdata>/input-data/soil/soil_geolog.csv"
)


paths <- kwb.utils::resolve(paths_list)


atmos <- kwb.hydrus1d::read_atmosph(paths$atmosphere)
View(atmos$data)

kwb.hydrus1d::run_model(model_path = paths$model_dir)

tlevel <- kwb.hydrus1d::read_tlevel(paths$t_level)
tlevel_aggr_date <- flextreat.hydrus1d::aggregate_tlevel(tlevel)

# Datum in Date-Format umwandeln
data <- atmos$data %>%
  dplyr::bind_cols(tibble::tibble(evapo_r_modelled = tlevel_aggr_date$evap[seq_len(nrow(atmos$data))])) %>%
  dplyr::mutate(store = dplyr::if_else(Prec - evapo_r_modelled < 0,
                                       1,
                                       0)) %>%
  dplyr::mutate(dplyr::across(tidyselect::starts_with("c"), ~ . * Prec, .names = "Prec_{.col}"))

#  mutate(date = as.Date(date))

# Funktion zum Identifizieren zusammenhängender Gruppen
data_grouped <- data %>%
  arrange(tAtm) %>%
  mutate(group = cumsum(store != dplyr::lag(store, default = dplyr::first(store))))

# Summiere "load_in" und bestimme Anfangs- und Enddatum
result <- data_grouped %>%
  #dplyr::filter(store == 1) %>%
  dplyr::group_by(group, store) %>%
  dplyr::summarize(
    tAtm_start = min(tAtm),
    tAtm_end = max(tAtm),
    tAtm_diff = as.integer(diff.Date(c(tAtm_start, tAtm_end))),
    Prec_sum = sum(Prec)
    #total_load_in = sum(load_in)
  )

result_store <- result %>%
  dplyr::filter(store == 1) %>%
  dplyr::select(group, Prec_sum) %>%
  dplyr::mutate(group = group + 1) %>%
  dplyr::rename(Prec_sum_stored = Prec_sum)

result_merged <- result %>%
  dplyr::left_join(result_store, by = "group") %>%
  dplyr::mutate(Prec_sum_stored = dplyr::if_else(is.na(Prec_sum_stored),
                                                 0,
                                                 Prec_sum_stored),
                Prec_cor = dplyr::if_else(Prec_sum > 0,
                                          (Prec_sum + Prec_sum_stored)/Prec_sum,
                                          1))

View(result_merged)


result_merged %>%
  dplyr::mutate(tAtm = purrr::map2(tAtm_start, tAtm_end, seq)) %>%
  tidyr::unnest(tAtm) %>%
  dplyr::select(group, store, tAtm, Prec_cor)

# Überprüfen der umstrukturierten Daten
print(data_long)
