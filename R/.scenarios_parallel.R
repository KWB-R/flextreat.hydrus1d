# Preface ----------------------------------------------------------------------
if (FALSE) {
  remotes::install_github("kwb-r/kwb.hydrus1d@dev")
  remotes::install_github("kwb-r/flextreat.hydrus1d@dev")
}

library(magrittr)
library(flextreat.hydrus1d)

# get_atm ----------------------------------------------------------------------
get_atm <- function(atm, extreme_rain = NULL)
{
  `%>%` <- magrittr::`%>%`

  if (is.null(extreme_rain)) {
    return(atm)
  }

  if (!extreme_rain %in% c("dry", "wet")) {
    stop("extreme_rain has to be either 'NULL', 'dry' or 'wet")
  }

  atm <- atm %>%
    dplyr::mutate(
      hydrologic_year = flextreat.hydrus1d::get_hydrologic_years(date),
      year = as.integer(format(date, format = "%Y")),
      day_of_year = lubridate::yday(date)
    )

  atm_stats <- atm %>%
    dplyr::group_by(year) %>%
    dplyr::summarise(
      rain_mm = sum(rain_mm, na.rm = TRUE),
      evapo_p_mean_mm = sum(evapo_p_mean_mm, na.rm = TRUE)
    )

  if (extreme_rain == "dry") {

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
  }

  if (extreme_rain == "wet") {
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
  }
}

# prepare_solute_input ---------------------------------------------------------
prepare_solute_input <- function(
    dat,
    selector,
    Ks = NULL,
    SnkL1 = NULL,
    diff_w = 0, diff_g = 0,
    kd = NULL,
    halftime_to_firstorderrate = NULL
)
{
  `%>%` <- magrittr::`%>%`

  stopifnot(nrow(dat) <= 10)

  if (selector$solute$No.Solutes != nrow(dat)) {
    selector$solute$No.Solutes <- nrow(dat)
  }

  solute_names <- sprintf("solute_%d", seq_len(nrow(dat)))

  solutes_new <- setNames(nm = solute_names, lapply(seq_len(nrow(dat)), function(i) {

    dat_sel <- dat[i,]

    ks <- kd(
      porosity = selector$waterflow$soil$ths - selector$waterflow$soil$thr,
      retardation = dat_sel$retard,
      bulk_density = selector$solute$transport$Bulk.d.
    )

    cols <- c(
      "Ks", "Nu", "Beta", "Henry", "SnkL1", "SnkS1", "SnkG1", "SnkL1'",
      "SnkS1'", "SnkG1'", "SnkL0",  "SnkS0", "SnkG0",  "Alfa"
    )

    reaction <- matrix(data = 0, ncol = length(cols), nrow = length(ks)) %>%
      as.data.frame() %>%
      tibble::as_tibble()

    names(reaction) <- cols
    reaction$Beta <- 1

    reaction$Ks <- if (is.null(Ks)) {
      round(ks, 2)
    } else {
      Ks
    }

    reaction$SnkL1 <- if (is.null(SnkL1)) {
      round(halftime_to_firstorderrate(dat_sel$half_life_days), 5)
    } else {
      SnkL1
    }

    list(
      diffusion = tibble::tibble(DifW = diff_w, DifG = diff_g),
      reaction = reaction
    )

  }))

  sel_tmp <- selector$solute[!names(selector$solute) %in% solute_names]

  solutes_new_list <- c(
    sel_tmp[1:which(names(sel_tmp) == "transport")],
    solutes_new,
    sel_tmp[which(names(sel_tmp) == "kTopSolute"):length(sel_tmp)]
  )

  c(
    selector[names(selector) != "solute"],
    list(solute = solutes_new_list)
  )
}

# get_mean ---------------------------------------------------------------------
get_mean <- function(col)
{
  x <- stringr::str_split_fixed(col, "-", n = 2)
  mode(x) <- "numeric"
  round(rowMeans(x), digits = 2)
}

# kd ---------------------------------------------------------------------------
kd <- function(porosity, retardation, bulk_density)
{
  #https://www3.epa.gov/ceampubl/learn2model/part-two/onsite/retard.html
  (retardation - 1) * porosity / bulk_density
}

# halftime_to_firstorderrate ---------------------------------------------------
halftime_to_firstorderrate <- function(half_time)
{
  if(half_time != 0) {
    #https://chem.libretexts.org/Courses/Bellarmine_University/BU%3A_Chem_104_(Christianson)/Phase_2%3A_Understanding_Chemical_Reactions/4%3A_Kinetics%3A_How_Fast_Reactions_Go/4.5%3A_First_Order_Reaction_Half-Life#mjx-eqn-21.4.2
    0.693 / half_time
  } else {
    0
  }
}

# provide_soil_columns ---------------------------------------------------------
provide_soil_columns <- function(path)
{
  `%>%` <- magrittr::`%>%`

  kwb.db::hsGetTable(path, "my_results2", stringsAsFactors = FALSE) %>%
    janitor::clean_names() %>%
    dplyr::mutate(half_life_days = dplyr::case_when(
      grepl(">", hwz_tage) ~ hwz_tage %>%
        stringr::str_remove(">") %>%
        as.numeric() %>%
        round(digits = 2),
      grepl("<", hwz_tage) ~ hwz_tage %>%
        stringr::str_remove("<") %>%
        as.numeric() %>%
        round(digits = 2),
      grepl("-", hwz_tage) ~ get_mean(hwz_tage),
      .default = as.numeric(hwz_tage) %>%
        round(digits = 2)),
      retard = dplyr::case_when(
        grepl("-", retardation) ~ get_mean(retardation),
        is.na(retardation) ~ 1L,
        .default = as.numeric(retardation) %>%
          round(digits = 2))) %>%
    dplyr::filter(!is.na(half_life_days)) %>%
    dplyr::mutate(id = 1:dplyr::n()) %>%
    dplyr::relocate(id) # %>% dplyr::mutate(retard = 1, half_life_days = 0)
}

# MAIN -------------------------------------------------------------------------

#path <- "Y:/WWT_Department/Projects/FlexTreat/Work-packages/AP3/3_1_2_Boden-Grundwasser/daten_karten/Sickerwasserprognose/column-studies/Stoffeigenschaften_Säulen.xlsx"
path <- "Y:/WWT_Department/Projects/FlexTreat/Work-packages/AP3/3_1_4_Prognosemodell/StofflicheModellrandbedingungen.xlsx"

soil_columns <- provide_soil_columns(path)

### Select 1 substance for 5 different half life classes defined in this table
selected_substances <- readr::read_csv("inst/extdata/input-data/substance_classes.csv")

soil_columns_selected <- soil_columns  %>%
  dplyr::filter(substanz_nr %in% selected_substances$substance_id) %>%
  dplyr::arrange(substanz_nr) %>%
  dplyr::mutate(id = 1:dplyr::n())

arg_combis <- kwb.utils::expandGrid(
  extreme_rain = c("", "wet", "dry"), # "" needs to be treated as NULL!
  treatment = c("tracer", "ka", "o3"), # "tracer" # c("ka", "o3")
  scenario = unlist(lapply(c(1,10), function(x) {
    paste0("soil-", 1:3, sprintf("m_irrig-%02ddays", x))
  })),
  irrig_only_growing_season = c(TRUE, FALSE),
  duration_string = "long", #c("short", "long"),
  retardation_scenario = c("retardation_yes", "retardation_no", "tracer")
)


# generate_solute_ids ----------------------------------------------------------
generate_solute_ids <- function(n)
{
  seq_start <- seq(1, n, 10)

  seq_end <- if (n > 10) {
    seq(10, n, 10)
  } else {
    n
  }

  if (length(seq_end) < length(seq_start)) {
    seq_end <- c(seq_end, n)
  }

  tibble::tibble(
    start = as.integer(seq_start),
    end = as.integer(seq_end)
  )
}

# generate_periods -------------------------------------------------------------
generate_periods <- function(n)
{
  tibble::tibble(
    start = seq(1, n, 10),
    end = if (n %% 10 != 0) {
      c(seq(10, n, 10), n)
    } else {
      seq(10, n, 10)
    }
  )
}

# prepare_files_for_irrig_int --------------------------------------------------
prepare_files_for_irrig_int <- function(paths)
{
  `%>%` <- magrittr::`%>%`

  p <- kwb.utils::createAccessor(paths)

  copy <- function(fun, from, to) {
    kwb.utils::catAndRun(
      sprintf("Copying from\n  %s to\n  %s", from, to),
      fun(path = from, new_path = to)
    )
  }

  if (!fs::dir_exists(p("model_dir"))) {
    copy(fs::dir_copy, from = p("model_dir_org"), to = p("model_dir"))
  }

  if (!fs::file_exists(p("model_gui_path"))) {
    copy(fun = fs::file_copy, from = p("model_gui_path_org"), to = p("model_gui_path"))
  }

  model_out_files <- list.files(
    p("model_dir"),
    pattern = "\\.out$",
    full.names = TRUE
  )

  if (length(model_out_files) > 0L) {
    fs::file_delete(model_out_files)
  }

  soil_depth_cm <- 100 *
    stringr::str_extract(p("model_name"), "soil-[0-9]+?m") %>%
    stringr::str_extract("[0-9]") %>%
    as.numeric()

  if (soil_depth_cm != 200) {
    soil_profile <- kwb.hydrus1d::read_profile(paths$profile)
    profile_extended <- kwb.hydrus1d::extend_soil_profile(
      soil_profile$profile,
      x_end = -soil_depth_cm
    )
    soil_profile_extended <- soil_profile
    soil_profile_extended$profile <- profile_extended
    kwb.hydrus1d::write_profile(soil_profile_extended, path = p("profile"))
  }

  string_irrig_int <- stringr::str_extract(p("model_dir"), "[0-9][0-9]?days")

  # Return the string that is used as "irrig_interval"
  paste(
    as.integer(stringr::str_extract(string_irrig_int, "\\d+")),
    stringr::str_extract(string_irrig_int, "[a-z]+")
  )
}

# sum_per_interval -------------------------------------------------------------
sum_per_interval <- function(data, interval)
{
  `%>%` <- magrittr::`%>%`

  data_org <- data
  data <- dplyr::select(data, tidyselect::all_of(
    c("date", "groundwater.mmPerDay", "clearwater.mmPerDay")
  ))
  cols_sum <- setdiff(names(data), "date")
  data_summed <- data %>%
    dplyr::mutate(
      group = lubridate::floor_date(date, unit = interval)
    ) %>%  # Konvertiere date in datetime-Format
    dplyr::group_by(group) %>%  # Gruppiere nach Zeitintervallen
    dplyr::summarise_at(
      .vars = tidyselect::all_of(cols_sum),
      .funs = sum
    ) %>%   # Berechne die Summe für jedes Intervall
    dplyr::rename(date = group)
  data_org[, cols_sum] <- 0
  data_org[data_org$date %in% data_summed$date, cols_sum] <- data_summed[, cols_sum]
  data_org
}

# get_valid_exe_path -----------------------------------------------------------
get_valid_exe_path <- function(exe_dir)
{
  if (file.exists(file.path(exe_dir, "H1D_CALC.exe"))) {
    file.path(exe_dir, "H1D_CALC.exe")
  } else {
    kwb.hydrus1d::check_hydrus_exe()
  }
}

# provide_paths ----------------------------------------------------------------
provide_paths <- function(config, start, end)
{
  #Y:\WWT_Department\Projects\FlexTreat\Work-packages\AP3\3_1_4_Prognosemodell\Hydrus1D\irrig_fixed\irrig-period_status-quo\long_dry\retardation_no
  tracer <- config$treatment == "tracer"
  # Define a path grammar
  PATH_GRAMMAR <- list(
    exe_dir = sprintf("C:/kwb/projects/flextreat/3_1_4_Prognosemodell/Vivian/Rohdaten/irrig_fixed/<irrig_dir_string>/<duration_string><extreme_rain_string>/%s", config$retardation_scenario),
    model_name_org = "model_to_copy",
    model_name = "<location>_<scenario>_soil-column_<solute_id_start><solute_id_end>",
    ###model_gui_path_org =  "<exe_dir>/<model_name_org>.h1d",
    model_gui_path_org =  "<model_dir_org>/<model_name_org>.h1d",

    model_gui_path = "<exe_dir>/<model_name>.h1d",
    modelvs_gui_path = "<exe_dir>/<model_name>_vs.h1d",

    model_dir_org = "C:/kwb/projects/flextreat/3_1_4_Prognosemodell/Vivian/Rohdaten/irrig_fixed/<model_name_org>",

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
    soil_data = "<extdata>/input-data/soil/soil_geolog.csv",
    solute_id_start = sprintf("%02d", start),
    solute_id_end = sprintf("%02d", end),
    location = if (tracer) {
      "tracer"
    } else {
      sprintf("ablauf_%s_median", config$treatment)
    } #"ablauf_ka_median"
  )

  # Resolve the path grammar by replacing the placeholders recursively
  kwb.utils::resolve(
    PATH_GRAMMAR,
    irrig_dir_string = ifelse(
      config$irrig_only_growing_season,
      "irrig-period_growing-season",
      "irrig-period_status-quo"
    ),
    duration_string = config$duration_string,
    extreme_rain_string = if (config$extreme_rain != "") {
      paste0("_", config$extreme_rain)
    } else {
      ""
    },
    #final_subdir = ifelse(tracer, "tracer", config$retardation_scenario),
    scenario = config$scenario
  )
}

# inner_function ---------------------------------------------------------------
inner_function <- function(config, atm_data, soil_columns, helper)
{
  `%>%` <- magrittr::`%>%`

  {
    # Define constants
    IRRIGATION_COLUMNS <- c("groundwater.mmPerDay", "clearwater.mmPerDay")

    # Provide variables from config
    extreme_rain <- if (config$extreme_rain == "") {
      NULL
    } else {
      config$extreme_rain
    }
    tracer <- config$treatment == "tracer"
    no_retardation <- config$retardation == "retardation_no"

    if (no_retardation) {
      soil_columns$retard <- 1
    }

    atm <- helper("get_atm")(atm_data, extreme_rain)

    if (config$irrig_only_growing_season) {
      atm[which(!lubridate::month(atm$date) %in% 4:9), IRRIGATION_COLUMNS] <- 0
    }

    if (config$duration_string == "test") {
      atm <- dplyr::filter(atm, date >= "2017-05-01" & date <= "2018-04-30")
    } else if (config$duration_string == "short") {
      atm <- dplyr::filter(atm, date >= "2017-05-01" & date <= "2020-04-30")
    } else {
      atm <- dplyr::filter(atm, date >= "2017-05-01" & date <= "2023-12-31")
    }

    days_monthly <- lubridate::days_in_month(
      seq.Date(from = min(atm$date), to = max(atm$date), by = "month")
    )

    loop_df <- if (tracer) {
      helper("generate_periods")(n = length(days_monthly))
    } else {
      helper("generate_solute_ids")(n = nrow(soil_columns))
    }
  }

  kwb.utils::catAndRun(
    messageText = sprintf(
      "Running '%s' period for treatment '%s'",
      extreme_rain,
      config$treatment
    ),
    expr = {
      sapply(seq_len(nrow(loop_df)), function(i) {

        #i <- 1L
        {
          paths <- helper("provide_paths")(
            config,
            start = loop_df$start[i],
            end = loop_df$end[i]
          )

          solute_start_id <- as.numeric(paths$solute_id_start)
          solute_end_id <- as.numeric(paths$solute_id_end)
          n_solutes <- solute_end_id - (solute_start_id - 1)

          no_irrig <- stringr::str_detect(paths$model_dir, "no-irrig")
          irrig_int <- stringr::str_detect(paths$model_dir, "irrig-[0-9][0-9]?days")
        }

        if (irrig_int) {
          irrig_interval <- helper("prepare_files_for_irrig_int")(paths)
        }

        # no-irrigation
        if (no_irrig) {
          atm[, IRRIGATION_COLUMNS] <- 0
        }

        if (irrig_int) {
          atm <- helper("sum_per_interval")(data = atm, interval = irrig_interval)
        }

        days_total <- cumsum(days_monthly)

        indices <- seq_along(days_total)

        c_tops <- lapply(indices, function(i) {

          x <- rep(0, nrow(atm))
          x_min <- ifelse(i == 1, 1, days_total[i - 1] + 1)
          x[x_min:days_total[i]] <- rep(100, days_monthly[i])

          tib <- data.frame(x)

          colnames(tib) <- if (i == indices[1]) {
            "cTop"
          } else {
            sprintf("cTop%d", which(indices %in% i))
          }

          tib
        }) %>% dplyr::bind_cols()

        c_tops_sel <- c_tops[, paths$solute_id_start:paths$solute_id_end]

        atm_prep <- if (tracer) {
          flextreat.hydrus1d::prepare_atmosphere(
            atm = atm,
            conc_irrig_clearwater = c_tops_sel,
            conc_irrig_groundwater = c_tops_sel,
            conc_rain = c_tops_sel
          )
        } else {
          flextreat.hydrus1d::prepare_atmosphere(
            atm = atm,
            conc_irrig_clearwater = soil_columns[solute_start_id:solute_end_id, paths$location],
            conc_irrig_groundwater = 0,
            conc_rain = 0
          )
        }

        n_tsteps <- nrow(atm_prep)

        writeLines(
          kwb.hydrus1d::write_atmosphere(atm = atm_prep),
          paths$atmosphere
        )

        selector <- kwb.hydrus1d::read_selector(path = paths$selector)

        selector$time$tMax <- n_tsteps
        selector$time$MPL <- 250

        selector$time$TPrint <- seq(0, n_tsteps, n_tsteps/selector$time$MPL)

        if (selector$time$TPrint[1] == 0) {
          selector$time$TPrint[1] <- 1
        }

        solutes_new <- helper("prepare_solute_input")(
          dat = soil_columns[solute_start_id:solute_end_id,],
          Ks = if (tracer) 0, # else NULL
          SnkL1 = if (tracer) 0, # else NULL
          selector = selector,
          diff_w = 0,
          diff_g = 0,
          kd = helper("kd"),
          halftime_to_firstorderrate = helper("halftime_to_firstorderrate")
        )

        kwb.hydrus1d::write_selector(solutes_new, paths$selector)

        hydrus1d <- kwb.hydrus1d::read_hydrus1d(paths$hydrus1d)
        hydrus1d$Main$NumberOfSolutes <- n_solutes
        kwb.hydrus1d::write_hydrus1d(hydrus1d, paths$hydrus1d)

        exe_path <- helper("get_valid_exe_path")(paths$exe_dir)

        kwb.hydrus1d::run_model(
          exe_path = exe_path,
          model_path = paths$model_dir,
          print_output = FALSE
        )

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

        if (length(model_vs_out_files) > 0) {
          fs::file_delete(model_vs_out_files)
        }

        writeLines(
          kwb.hydrus1d::write_atmosphere(atm = atmos$data),
          paths$atmosphere_vs
        )

        kwb.hydrus1d::run_model(
          exe_path = exe_path,
          model_path = paths$model_dir_vs,
          print_output = FALSE
        )

      })
    },
    dbg = TRUE
  )
}

# helper -----------------------------------------------------------------------
helper <- kwb.utils::createAccessor(list(
  get_atm = get_atm,
  generate_solute_ids = generate_solute_ids,
  provide_paths = provide_paths,
  prepare_files_for_irrig_int = prepare_files_for_irrig_int,
  sum_per_interval = sum_per_interval,
  prepare_solute_input = prepare_solute_input,
  halftime_to_firstorderrate = halftime_to_firstorderrate,
  get_valid_exe_path = get_valid_exe_path,
  generate_periods = generate_periods,
  kd = kd
))

# Main loop --------------------------------------------------------------------

if (FALSE) {

  atm_data <- flextreat.hydrus1d::prepare_atmosphere_data()


  dirs <- fs::dir_ls("C:/kwb/projects/flextreat/3_1_4_Prognosemodell/Vivian/Rohdaten/irrig_fixed",
                     recurse = TRUE,
                     regexp = "retardation_yes",
                     type = "directory")

  dirinfo <- fs::dir_info(path = dirs, type = "directory")

  dirinfo <- dirinfo[dirinfo$change_time <  as.POSIXct("2024-10-17 14:30:00"),]

  mod_scens <- dirinfo$path %>%
    stringr::str_remove("C:/kwb/projects/flextreat/3_1_4_Prognosemodell/Vivian/Rohdaten/irrig_fixed/") %>%
    stringr::str_split_fixed("/", 4) %>% as.data.frame()

  names(mod_scens) <- c("irrig_only_growing_season", "duration_string_extreme_rain", "retardation_scenario", "treatment_scenario")

  mod_scens <- mod_scens %>%
    tidyr::separate(duration_string_extreme_rain, c("duration_string", "extreme_rain"), sep = "_") %>%
    dplyr::mutate(extreme_rain = dplyr::if_else(is.na(extreme_rain), "", extreme_rain),
                  irrig_only_growing_season = dplyr::if_else(irrig_only_growing_season == "irrig-period_growing-season",
                                                               TRUE,
                                                               FALSE),
                  treatment = stringr::str_extract(treatment_scenario, "ka|o3"),
                  scenario = treatment_scenario %>%  stringr::str_remove(".*_median_") %>%
                    stringr::str_remove("_soil.*"))

  #arg_combis <- arg_combis[arg_combis$scenario == "soil-3m_irrig-10days" & arg_combis$irrig_only_growing_season == FALSE & arg_combis$duration_string == "long" & arg_combis$retardation_scenario == "tracer" & arg_combis$treatment == "tracer" & arg_combis$extreme_rain == "", ]
  #arg_combis <- arg_combis[arg_combis$treatment != "tracer" & arg_combis$retardation_scenario != "tracer" | arg_combis$treatment == "tracer" & arg_combis$retardation_scenario == "tracer", ]
  #arg_combis <- arg_combis[arg_combis$retardation_scenario == "tracer" & arg_combis$treatment == "tracer" & arg_combis$scenario == "soil-1m_irrig-10days" & arg_combis$duration_string == "long" & arg_combis$irrig_only_growing_season == FALSE & arg_combis$extreme_rain == "",]
  #arg_combis <- arg_combis[arg_combis$retardation_scenario == "tracer" & arg_combis$treatment == "tracer" & arg_combis$scenario != "soil-1m_irrig-01days" & arg_combis$duration_string == "long" & arg_combis$irrig_only_growing_season == TRUE & arg_combis$extreme_rain == "",]
  #arg_combis <- arg_combis[arg_combis$retardation_scenario == "retardation_no" & arg_combis$treatment  %in% c("ka", "o3"),]
  #arg_combis <- mod_scens
  arg_combis <- kwb.utils::expandGrid(
    extreme_rain = c("", "wet", "dry"), # "" needs to be treated as NULL!
    treatment = c("ka", "o3"), # "tracer" # c("ka", "o3")
    scenario = unlist(lapply(1, function(x) {
      paste0("soil-", 1:3, sprintf("m_irrig-%02ddays", x))
    })),
    irrig_only_growing_season = c(TRUE, FALSE),
    duration_string = "long", #c("short", "long"),
    retardation_scenario = "retardation_no")

  arg_combis <- kwb.utils::expandGrid(
    extreme_rain = c(""), # "" needs to be treated as NULL!
    treatment = c("ka"), # "tracer" # c("ka", "o3")
    scenario = unlist(lapply(1, function(x) {
      paste0("soil-", 1, sprintf("m_irrig-%02ddays", x))
    })),
    irrig_only_growing_season = c(FALSE),
    duration_string = "long", #c("short", "long"),
    retardation_scenario = "retardation_no")

  arg_combis <- kwb.utils::expandGrid(
    extreme_rain = c("", "wet", "dry"), # "" needs to be treated as NULL!
    treatment = c("ka", "o3"), # "tracer" # c("ka", "o3")
    scenario = unlist(lapply(c(1,10), function(x) {
      paste0("soil-", 1:3, sprintf("m_irrig-%02ddays", x))
    })),
    irrig_only_growing_season = c(TRUE, FALSE),
    duration_string = "long", #c("short", "long"),
    retardation_scenario = "retardation_yes")


  configs <- lapply(seq_len(nrow(arg_combis)), function(i) {
    as.list(arg_combis[i, ])
  })

  # Sequential loop
  for (config in configs) {
    #config <- configs[[1L]]
    inner_function(
      config = config,
      atm_data = atm_data,
      soil_columns = soil_columns_selected,
      helper = helper
    )
  }

  # Parallel loop
  ncores <- parallel::detectCores()
  ncores <- 8
  cl <- parallel::makeCluster(ncores)


  parallel::parLapply(
    cl = cl,
    X = configs,
    fun = inner_function,
    atm_data = atm_data,
    soil_columns = soil_columns_selected,
    helper = helper
  )

  parallel::stopCluster(cl)
}

# After main loop --------------------------------------------------------------
if (FALSE)
{
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
    dplyr::filter(
      subdomain_id == 0,
      time < 400,
      parameter == "CncBalT"
    ) %>%
    ggplot2::ggplot(
      mapping = ggplot2::aes(x = time, y = value, col = as.factor(solute_id))
    ) + ggplot2::geom_point()

  balance %>%
    dplyr::filter(
      subdomain_id == 0,
      time < 400,
      parameter == "CncBalR"
    ) %>%
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
    ggplot2::ggplot(mapping = ggplot2::aes(
      x = time,
      y = value,
      col = as.factor(x))
    ) +
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

  tlevel_aggr_yearmonth <- flextreat.hydrus1d::aggregate_tlevel(
    t_level,
    col_aggr = "yearmonth"
  )

  tlevel_aggr_year_hydrologic <- flextreat.hydrus1d::aggregate_tlevel(
    t_level,
    col_aggr = "year_hydrologic"
  ) %>%
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

  #root_path <- "D:/hydrus1d/irrig_fixed_01"
  root_path <- "C:/kwb/projects/flextreat/3_1_4_Prognosemodell/Vivian/Rohdaten/irrig_fixed"

  scenario_dirs <- fs::dir_ls(
    path =   root_path,
    recurse = TRUE,
    #regexp = "retardation_no.*vs$",
    regexp = "irrig-period_status-quo/long/retardation_no.*vs$",
    #regexp = "irrig-period_status-quo/long/retardation_no/ablauf_ka_median_soil-1m_irrig-01days_soil-column_0105.*vs$",
    type = "directory"
  )

  # Set up parallel plan
  system.time(expr = {
    # future::plan(future::multisession)

    future.apply::future_sapply(scenario_dirs, function(scenario_dir) {

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
          dplyr::left_join(soil_columns_selected, by = c(soil_column_id = "id"))


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
    # Close the parallel plan
    future::plan(future::sequential)
  })

  retardation_short <- "retardation_yes"

  soil_columns_plot <- if(retardation_short == "retardation_yes") {
    soil_columns_selected
  } else {
    soil_columns_selected %>%
      dplyr::mutate(retard = 1)
  }

  scenario_dirs <- fs::dir_ls(
    path =   root_path,
    recurse = TRUE,
    #regexp = "irrig-period_growing-season/long/retardation_no/.*vs/hydrus_scenarios.xlsx$",
    regexp = sprintf("%s.*vs/hydrus_scenarios.xlsx$", retardation_short),
    type = "file"
  )

  #fs::file_info(scenario_dirs) %>% View()


  res_stats <- lapply(
    stats::setNames(nm = scenario_dirs),
    function(scenario_dir) {
      readxl::read_excel(scenario_dir)
    }
  )

 # load_default <- res_stats$`D:/hydrus1d/irrig_fixed_01/irrig-period_status-quo/long/retardation_no/ablauf_ka_median_soil-2m_irrig-10days_soil-column_0105_vs/hydrus_scenarios.xlsx`
  load_default <- res_stats[[sprintf("C:/kwb/projects/flextreat/3_1_4_Prognosemodell/Vivian/Rohdaten/irrig_fixed/irrig-period_status-quo/long/%s/ablauf_ka_median_soil-2m_irrig-10days_soil-column_0105_vs/hydrus_scenarios.xlsx",
                                     retardation_short)]] %>%
    # dplyr::mutate(retardation = basename(dirname(dirname(path))),
    #               duration = basename(dirname(dirname(dirname(path)))),
    #               irrigation_period = basename(dirname(dirname(dirname(dirname((path))))))) %>%
    dplyr::select(- path, - scen, - mass_balance_error_percent, - soil)

  names(load_default)[3:5] <- paste0("default_",   names(load_default)[3:5])


  res_stats_df <- dplyr::bind_rows(res_stats) %>%
    dplyr::mutate(retardation = basename(dirname(dirname(path))),
                  duration = basename(dirname(dirname(dirname(path)))),
                  irrigation_period = basename(dirname(dirname(dirname(dirname((path))))))) %>%
    dplyr::select(- path, - mass_balance_error_percent, - soil)


  res_stats_df <- res_stats_df %>%
    dplyr::left_join(load_default, by = c("substanz_nr", "substanz_name")) %>%
    dplyr::mutate(percental_load_gw = dplyr::if_else(abs(default_sum_cv_bot) < 10000 | abs(sum_cv_bot) < 10000,
                                                     NA_real_,
                                                     100 + 100 * (abs(sum_cv_bot) - abs(default_sum_cv_bot)) /  abs(default_sum_cv_bot))) %>%
    tidyr::separate(col = scen, into = c("treatment", "soil_depth_irrig"), sep = "n_s") %>%
    dplyr::mutate(treatment = sprintf("%sn", treatment),
                  soil_depth_irrig = sprintf("s%s", soil_depth_irrig)) %>%
    tidyr::separate(col = soil_depth_irrig, into = c("soil_depth", "irrigation_intervall"), sep = "_", remove = FALSE) %>%
    dplyr::mutate(duration_irrigperiod = paste0(duration, "_", irrigation_period))

  de <- TRUE


  if(de) {
  res_stats_df$duration_irrigperiod <- res_stats_df$duration_irrigperiod %>%
      stringr::str_replace("long_wet", "DWD, trocken (2013: 380 mm/a)") %>%
      stringr::str_replace("long_dry", "DWD, nass    (2023: 835 mm/a)") %>%
      stringr::str_replace("long", "DWD   (2017-2023: 611 mm/a)") %>%
      stringr::str_replace("_irrig-period_status-quo", ", Bewässerung: ganzjährig") %>%
      stringr::str_replace("_irrig-period_growing-season", ", Bewässerung: nur Vegetationsperiode (Apr-Sep)")

    res_stats_df$soil_depth_irrig <- res_stats_df$soil_depth_irrig %>%
      stringr::str_replace("soil-", "Boden ") %>%
      stringr::str_replace("_irrig-10days", ", Bewässerung: zehntägig") %>%
      stringr::str_replace("_irrig-01days", ", Bewässerung: täglich")
  lang_id <- "de"
  lab_x <- "Bodenm\u00E4chtigkeit und Bew\u00E4sserungsintervall"
  lab_y <- "Prozentuale Fracht ins Grundwasser i.V. zu Status Quo (%)"
  title <- "Stoff: %s (Halbwertszeit: %3.1f Tage, Retardation: %2.1f)"
  legend_col <- "Klima- und Bew\u00E4sserungsszenario "
  legend_shape <- "Aufbereitung"
  treatment_wwtp <- "Kl\u00E4ranlagenablauf   (Median: "
  treatment_o3 <- "Ozonanlagenablauf (Median: "
  treatment_unit <- "ng/l"


  } else {
    lang_id <- "en"
    lab_x <- "Scenario"
    lab_y <- "Percental Load to Groundwater compared to Status Quo (%)"
    title <- "Substance: %s (half life time: %3.1f days, retardation: %2.1f)"
    legend_col <- "Scenario"
    legend_shape <- "Treatment"
    treatment_wwtp <- "WWTP effluent (median: "
    treatment_o3 <- "Ozone effluent (median: "
    treatment_unit <- "ng/l"
  }

  #View(res_stats_df)


  substances <- unique(res_stats_df$substanz_name)[order(unique(res_stats_df$substanz_name))]


  pdff <- sprintf("percental-load-to-groundwater_per-substance_%s_%s.pdf",
                  stringr::str_replace(retardation_short, "_", "-"),
                  lang_id)
  kwb.utils::preparePdf(pdff, borderHeight.cm = 0,  borderWidth.cm = 0, width.cm = 32.67, height.cm = 23.1)

  sapply(substances, function(substance) {

   substance_meta  <- soil_columns_plot[soil_columns_plot$substanz_name == substance,]

    res_stats_df_sel <-  res_stats_df %>%
      dplyr::filter(substanz_name == substance)

    res_stats_df_sel$treatment_conc <- ""

    res_stats_df_sel$treatment_conc[which(res_stats_df_sel$treatment == "ablauf_ka_median")] <- sprintf("%s %5d %s)", treatment_wwtp, round(substance_meta$ablauf_ka_median, 0),   treatment_unit)
    res_stats_df_sel$treatment_conc[which(res_stats_df_sel$treatment == "ablauf_o3_median")] <- sprintf("%s %5d %s)", treatment_o3, round(substance_meta$ablauf_o3_median, 0),   treatment_unit)

  gg <- res_stats_df_sel %>%
    ggplot2::ggplot(mapping = ggplot2::aes(x = soil_depth_irrig,
                                         y = percental_load_gw,
                                         col = duration_irrigperiod,
                                         shape = treatment_conc
                                         #col = treatment
                                         )) +
  #ggplot2::geom_point(size = 3) +
  ggplot2::geom_jitter(width = 0.15, size = 3, alpha = 0.5) +
  #ggplot2::scale_y_log10(limits = c(1,1000)) +
  ggplot2::ylim(c(0,200)) +
    ggplot2::labs(y = lab_y, x = lab_x,
                  title = sprintf(title,
                                  substance_meta$substanz_name,
                                  substance_meta$half_life_days,
                                  substance_meta$retard),
                  col = legend_col,
                  shape = legend_shape) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "bottom",
                   legend.direction = "vertical",
                   legend.margin= ggplot2::margin(),
                   axis.text.x =  ggplot2::element_text(angle = 90, vjust = 0.5, hjust=1)#,
                   #axis.title.y = ggplot2::element_text(vjust = -10, margin = ggplot2::margin(t = 0, r = 0, b = 100, l = 0))
                   )
  print(gg)
})

kwb.utils::finishAndShowPdf(pdff)


pdff <- sprintf("percental-load-to-groundwater_per-scenario_%s.pdf",
                sprintf(retardation_short, "_", "-"))

kwb.utils::preparePdf(pdff)

sapply(duration_irrigperiods, function(duration_irrigper) {

  res_stats_df_sel <-  res_stats_df %>%
    dplyr::filter(duration_irrigperiod == duration_irrigper)

  gg <- res_stats_df_sel %>%
    ggplot2::ggplot(mapping = ggplot2::aes(x = soil_depth_irrig,
                                           y = percental_load_gw,
                                           col = substanz_name,
                                           shape = treatment
                                           #col = treatment
    )) +
    ggplot2::geom_point(size = 3) +
    ggplot2::labs(y = "Percental Load to Groundwater compared to Status Quo (%)", x = "Scenario",
                  title = sprintf("Scenario: %s %s (%s)",
                                  res_stats_df_sel$irrigation_period[1],
                                  res_stats_df_sel$duration[1],
                                  res_stats_df_sel$retardation[1]),
                  col = "Substance Name") +
    #ggplot2::scale_y_log10(limits = c(0,200)) +
    ggplot2::ylim(c(0,200)) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "top",
                   axis.text.x =  ggplot2::element_text(angle = 90, vjust = 0.5, hjust=1))
  print(gg)
})

kwb.utils::finishAndShowPdf(pdff)

 #root_path <- "D:/hydrus1d/irrig_fixed_01"
 root_path <- "C:/kwb/projects/flextreat/3_1_4_Prognosemodell/Vivian/Rohdaten/irrig_fixed"

  model_paths <- fs::dir_ls(
    path =   root_path,
    recurse = TRUE,
    regexp = "tracer$",
    type = "directory"
  )

  #model_paths <- model_paths[stringr::str_detect(model_paths, "wet|dry", negate = TRUE)]

  scenarios <- sapply(c(1,10), function(x) paste0("soil-", 1:3, sprintf("m_irrig-%02ddays", x))) %>%
    as.vector()

  #unique(configs$scenario)

  #traveltimes_list$`D:/hydrus1d/irrig_fixed_01/irrig-period_status-quo/long/tracer`[!sapply(traveltimes_list$`D:/hydrus1d/irrig_fixed_01/irrig-period_status-quo/long/tracer`, function(x) any(class(x) == "try-error"))]

  ### read traveltimes sequential
  system.time(
  traveltimes_list <- lapply(model_paths, function(model_path) {
    setNames(nm = (scenarios), lapply(scenarios, function(scenario) {
      try({
        message(sprintf("Scenario: %s", scenario))
        solute_files <- fs::dir_ls(
          path = model_path,
          recurse = TRUE,
          regexp = sprintf("tracer_%s_.*vs/solute\\d\\d?.out", scenario)
        )
        flextreat.hydrus1d::get_traveltimes(solute_files, dbg = TRUE)
      })
    }))
  }))

  ### read traveltimes in parallel
  library(future.apply)

  # Set up parallel plan
  system.time(expr = {
  future::plan(future::multisession)

  traveltimes_list <- future.apply::future_lapply(model_paths, function(model_path) {
    setNames(nm = (scenarios), future.apply::future_lapply(scenarios, function(scenario) {
      try({
        message(sprintf("Scenario: %s", scenario))
        solute_files <- fs::dir_ls(
          path = model_path,
          recurse = TRUE,
          regexp = sprintf("tracer_%s_.*vs/solute\\d\\d?.out", scenario)
        )
        flextreat.hydrus1d::get_traveltimes(solute_files, dbg = TRUE)
      })
    }))
  })
  }
  )

  # Close the parallel plan
  future::plan(future::sequential)
  traveltimes_list_backup <-  traveltimes_list

  sapply(seq_along(traveltimes_list), function(i) {

    htmlwidgets::saveWidget(flextreat.hydrus1d::plot_traveltimes(traveltimes_list[[i]] %>% dplyr::bind_rows(),
                                                                 title = sprintf("%s", extrahiere_letzte_drei_teile(names(traveltimes_list)[i])),
                                                                 ylim = c(0,650)),
                            file = sprintf("traveltimes_%s.html", names(traveltimes_list)[i]))
  })


  # extrahiere_letzte_drei_teile -------------------------------------------------
  extrahiere_letzte_drei_teile <- function(pfad)
  {
    sapply(pfad, function(pf) {
      # Teile den Pfad anhand des Schrägstrichs auf
      teile <- unlist(strsplit(pf, "/"))

      # Waehle die letzten drei Teile aus
      letzte_drei_teile <- tail(teile, 3)

      paste0(letzte_drei_teile, collapse = "_")
    })
  }

  # Plotting ---------------------------------------------------------------------
  if (FALSE)
  {
    pdff <- "traveltimes_per-scenario.pdf"
    kwb.utils::preparePdf(pdff)
    # sapply(names(traveltimes_list), function(path) {

      # traveltimes_sel <- traveltimes_list[[path]] %>% dplyr::bind_rows()
      #
      # label <-  extrahiere_letzte_drei_teile(path)

      # traveltime_bp <- traveltimes_sel %>%
      #       dplyr::bind_rows() %>%
      #       dplyr::filter(percentiles == 0.5) %>%
      #   dplyr::bind_rows(.id = "scenario") %>%
      #   dplyr::filter(!stringr::str_detect(scenario, "1.5")) %>%
      #   dplyr::mutate(quarter = lubridate::quarter(date) %>% as.factor(),
      #                 soil_depth =  stringr::str_extract(scenario, "soil-.*m") %>%
      #                   stringr::str_remove_all("soil-|m") %>%  as.factor())

      traveltime_bp <- lapply(traveltimes_list, function(x) {
        x %>%
          dplyr::bind_rows() %>%
          dplyr::filter(percentiles == 0.5)
      }) %>% dplyr::bind_rows(.id = "scenario") %>%
        dplyr::filter(!stringr::str_detect(scenario, "1.5")) %>%
        dplyr::mutate(quarter = lubridate::quarter(date) %>% as.factor(),
                      soil_depth =  stringr::str_extract(scenario, "soil-.*m") %>%
                        stringr::str_remove_all("soil-|m") %>%  as.factor())


      scenario_by_median_traveltime <- traveltime_bp %>%
        dplyr::group_by(scenario) %>%
        dplyr::summarise(median = median(time_diff, na.rm = TRUE)) %>%
        dplyr::arrange(median)

      traveltime_bp <- traveltime_bp %>%
        dplyr::left_join(scenario_by_median_traveltime)

      y_lim <- c(0,350)


      tt_bp_total <- traveltime_bp %>%
        dplyr::mutate(scenario_short = extrahiere_letzte_drei_teile(scenario)) %>%
        ggplot2::ggplot(ggplot2::aes(x = forcats::fct_reorder(scenario_short, median), y = time_diff)) +
        ggplot2::geom_boxplot(outliers = FALSE) +
        ggplot2::geom_jitter(position = ggplot2::position_jitter(width = 0.1),
                             col = "darkgrey",
                             alpha = 0.6) +
        ggplot2::ylim(y_lim) +
        ggplot2::labs(y = "Median Traveltime (days)", x = "Scenario",
                      title = "Boxplot: median traveltime total") +
        ggplot2::theme_bw() +
        ggplot2::theme(axis.text.x =  ggplot2::element_text(angle = 90, vjust = 0.5, hjust=1))

      print(tt_bp_total)


    kwb.utils::finishAndShowPdf(pdff)

    pdff <- "traveltimes_all.pdf"
    kwb.utils::preparePdf(pdff)

    traveltimes_df <- lapply(traveltimes_list, function(sublist) sublist %>% dplyr::bind_rows()) %>%
      dplyr::bind_rows(.id = "scenario_main_raw") %>%
      dplyr::mutate(
        scenario_name = stringr::str_remove_all(model_name, "_soil-column_.*vs$") %>% stringr::str_remove_all("tracer_"),
        scenario_main =  scenario_main_raw %>% extrahiere_letzte_drei_teile(),
        quarter = lubridate::quarter(date) %>% as.factor(),
        soil_depth =  stringr::str_extract(scenario_name, "soil-.*m") %>%
          stringr::str_remove_all("soil-|m") %>%  as.factor())

    scenario_by_median_traveltime <- traveltimes_df %>%
      dplyr::group_by(scenario_main, scenario_name) %>%
      dplyr::summarise(median = median(time_diff, na.rm = TRUE)) %>%
      dplyr::arrange(median)

    traveltimes_bp <- traveltimes_df %>%
      dplyr::left_join(scenario_by_median_traveltime)

    y_lim <- c(0,350)

    tt_bp_total <- traveltimes_bp %>%
      ggplot2::ggplot(ggplot2::aes(x = forcats::fct_reorder(scenario_name, median),
                                   y = time_diff,
                                   col = scenario_main)) +
      ggplot2::geom_boxplot(outliers = FALSE) +
      # ggplot2::geom_jitter(position = ggplot2::position_jitterdodge(
      #   jitter.width = 0.1,
      #   dodge.width = 0.75),
      #   alpha = 0.6) +
      ggplot2::ylim(y_lim) +
      ggplot2::labs(y = "Median Traveltime (days)",
                    x = "Scenario",
                    title = "Boxplot: median traveltime total") +
      ggplot2::theme_bw() +
      ggplot2::theme(legend.position = "top")

    print(tt_bp_total)

    kwb.utils::finishAndShowPdf(pdff)

    htmlwidgets::saveWidget(
      widget = plotly::ggplotly(tt_bp_total),
      file = "traveltimes_all.html"
    )

    pdff <- "traveltimes_all_percent.pdf"
    kwb.utils::preparePdf(pdff)

    traveltimes_df <- lapply(traveltimes_list, function(sublist) sublist %>% dplyr::bind_rows()) %>%
      dplyr::bind_rows(.id = "scenario_main_raw") %>%
      dplyr::mutate(
        scenario_name = stringr::str_remove_all(model_name, "_soil-column_.*vs$") %>% stringr::str_remove_all("tracer_"),
        scenario_main =  scenario_main_raw %>% extrahiere_letzte_drei_teile(),
        quarter = lubridate::quarter(date) %>% as.factor(),
        soil_depth =  stringr::str_extract(scenario_name, "soil-.*m") %>%
          stringr::str_remove_all("soil-|m") %>%  as.factor())

    scenario_base_median <- traveltimes_df %>%
      dplyr::filter(
        scenario_name == "soil-2m_irrig-10days",
        scenario_main == "irrig-period_status-quo_long_tracer",
        percentiles == 0.5
      ) %>%
      dplyr::select(- time_top, - time_bot) %>%
      dplyr::rename(time_diff_base = time_diff)

    traveltimes_bp <- traveltimes_df %>%
      dplyr::filter(percentiles == 0.5) %>%
      dplyr::left_join(scenario_base_median[, c("month_id", "time_diff_base")] %>% dplyr::mutate(percentiles = 0.5)) %>%
      dplyr::mutate(time_diff_percent = 100 + 100 * (time_diff - time_diff_base) / time_diff_base)

  # Plotting ---------------------------------------------------------------------

  y_lim <- c(0,350)

  tt_bp_percent <- traveltimes_bp %>%
    ggplot2::ggplot(ggplot2::aes(x = forcats::fct_reorder(scenario_name, time_diff_percent),
                                 y = time_diff,
                                 col = scenario_main)) +
    ggplot2::geom_boxplot(outliers = FALSE) +
    # ggplot2::geom_jitter(position = ggplot2::position_jitterdodge(
    #   jitter.width = 0.1,
    #   dodge.width = 0.75),
    #   alpha = 0.6) +
    ggplot2::ylim(y_lim) +
    ggplot2::labs(y = "Median Traveltime (%) compared to Status Quo",
                  x = "Scenario",
                  title = "Boxplot: median traveltime percent") +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "top")

  print(tt_bp_percent)

  kwb.utils::finishAndShowPdf(pdff)

  htmlwidgets::saveWidget(
    widget = plotly::ggplotly(tt_bp_total),
    file = "traveltimes_all.html"
  )

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

  # Save HTML widgets ------------------------------------------------------------
  htmlwidgets::saveWidget(
    widget = plotly::ggplotly(tt_bp_total),
    title = "Boxplot: median traveltime total",
    file = "boxplot_traveltimes-median_total.html"
  )

  htmlwidgets::saveWidget(
    plotly::ggplotly(tt_bp_quarter),
    title = "Boxplot: median traveltime by quarter",
    file = "boxplot_traveltimes-median_quarter.html"
  )

  }

### check model

  model_dir <- "C:/kwb/projects/flextreat/3_1_4_Prognosemodell/Vivian/Rohdaten/irrig_fixed"

  atm_files <- fs::dir_ls(model_dir, recurse = TRUE, type = "file", regexp = "tracer.*ATMOSPH.IN")

  atm_df <- lapply(atm_files, function(file) {
    atm <- kwb.hydrus1d::read_atmosph(file)
    tibble::tibble(path = file,
                   Prec_sum = sum(atm$data$Prec, na.rm = TRUE))
  }) %>% dplyr::bind_rows()

  path_split <- stringr::str_split_fixed(atm_df$path,"/", 13)

  atm_df %>%
    dplyr::mutate(irrig_period = path_split[,9],
                  duration_extreme = path_split[,10],
                  scenario= path_split[,12] %>% stringr::str_remove("_soil-column_.*")) %>%
    dplyr::group_by(irrig_period, duration_extreme, scenario) %>%
    dplyr::summarise(Prec_mean = sum(Prec_sum)/dplyr::n()) %>%
    View()


}
