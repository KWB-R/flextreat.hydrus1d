
#' Helper function: copy column
#'
#' @param df data fram
#' @param column name of colum to copy
#' @param number_of_new_columns  number of columns to be copied
#'
#' @return df with copied column names
#' @export
#' @examples
#' # Beispiel-Tibble
#' tb <- tibble::tibble(
#' ID = 1:5,
#' Wert = c(10, 20, 30, 40, 50)
#' )
#'
#' # Anwendung der Funktion
#' tb_neu <- copy_column(tb, "Wert", 3)
#' print(tb_neu)
#'
copy_column <- function(df, column, number_of_new_columns) {
  # Überprüfen, ob die angegebene Spalte existiert
  if (!(column %in% names(df))) {
    stop("The selected column does not exist in tibble.")
  }

  # Erstellen der neuen Spaltennamen
  new_columns <- paste(column , (1:number_of_new_columns) + 1, sep = "_")

  # Kopieren der Werte in die neuen Spalten
  for (new_column in new_columns) {
    df[[new_column]] <- df[[column]]
  }

  return(df)
}



#' Helper function: add cBot columns
#'
#' @param data data frame
#'
#' @return data frame with cBot columns
#' @keywords internal

add_cBot_columns <- function(data) {
  # Neue Spaltennamen erstellen
  original_columns <- names(data)
  new_columns <- unlist(lapply(seq_along(original_columns), function(i) {
    c(original_columns[i], paste0("cBot", ifelse(i == 1, "", i)))
  }))

  # Neue cBot-Spalten mit Nullen hinzufügen
  for (i in seq_along(original_columns)) {
    cBot_name <- paste0("cBot", ifelse(i == 1, "", i))
    data[[cBot_name]] <- 0
  }

  # Spalten neu anordnen
  data <- data %>% dplyr::select(tidyselect::all_of(new_columns))

  return(data)
}


#' Prepare Atmosphere
#'
#' Prepares atmospheric input data structure required by HYDRUS1D and by default
#' uses a conservative tracer in irrigation source "clearwater" ( set to 1) in
#' order to track the share of cleaned wastewater in the system inflow rate (as
#' "Prec" column is a combined value of irrigation using either "groundwater" or
#' "clearwater" and real "rainfall").
#'
#' @param atm  atm as retrieved by \code{prepare_atmosphere_data}
#' @param conc_irrig_clearwater substance concentration in source "clearwater"
#' used for irrigation (default: 100, set all other source concentrations in default
#' to 0 in order to calculate share of "clearwater" infiltration to groundwater)
#' @param conc_irrig_groundwater substance concentration in source "groundwater"
#' used for irrigation (default: 0)
#' @param conc_rain substance concentration in rainfall (default: 0)
#' @param defaults defaults for undefined parameters[kwb.hydrus1d::defaults_atmosphere()]
#' @return tibble with peoered
#' @export
#' @importFrom dplyr mutate rename select
#' @importFrom tidyselect all_of
#' @importFrom kwb.hydrus1d defaults_atmosphere
#' @importFrom tidyr replace_na
#' @examples
#'atm <- prepare_atmosphere_data()
#'atm_selected <- select_hydrologic_years(atm)
#'prepare_atmosphere(atm_selected)
prepare_atmosphere <- function(
    atm,
    conc_irrig_clearwater = 100,
    conc_irrig_groundwater = 0,
    conc_rain = 0,
    defaults = kwb.hydrus1d::defaults_atmosphere()
)
{
  inputs <- atm %>%
    dplyr::mutate(
      rain.cmPerDay = tidyr::replace_na(.data$rain_mm, 0) / 10,
      groundwater.cmPerDay = tidyr::replace_na(.data$groundwater.mmPerDay, 0) / 10,
      clearwater.cmPerDay = tidyr::replace_na(.data$clearwater.mmPerDay, 0) / 10,
      total_inflow.cmPerDay = rain.cmPerDay + clearwater.cmPerDay + groundwater.cmPerDay)


  conc_paras <- list(conc_irrig_groundwater = conc_irrig_groundwater,
       conc_irrig_clearwater = conc_irrig_clearwater,
       conc_rain = conc_rain)


  is_vector <- sapply(conc_paras, is.vector)

  n_conc <- sapply(seq_along(conc_paras), function(i) { if(is_vector[i]) {
    length(conc_paras[[i]])
    } else {
      if(is.data.frame(conc_paras[[i]])) {
        stopifnot(nrow(conc_paras[[i]]) == nrow(atm))
        stopifnot(ncol(conc_paras[[i]]) <= 10)
        ncol(conc_paras[[i]])
      } else {
        stop("unsupported data type")
      }
    }
  })

  names(n_conc) <- names(conc_paras)


  if (sum(n_conc > 1) > 1) {
    if(all(n_conc[n_conc > 1] != n_conc[n_conc > 1][1])) {
      stop(sprintf("Only length of one or identical lengths are allowed.\nHowever the parameters are specified as follows:\n%s",
                   paste0(names(n_conc[n_conc > 1]), " (n_conc = ", n_conc[n_conc > 1], ")", collapse = ",\n")))
    }
  }

  load_irrig_gw <- setNames(lapply(conc_irrig_groundwater,
                                   function(x) x * inputs$groundwater.cmPerDay),
                            nm = sprintf("load_irrig_gw_%d", seq_len(length(conc_irrig_groundwater)))) %>%
    dplyr::bind_cols()

  if(n_conc[["conc_irrig_groundwater"]] == 1 & max(n_conc) > 1) {
    load_irrig_gw <- copy_column(load_irrig_gw,
                column = names(load_irrig_gw),
                number_of_new_columns = max(n_conc) - 1)
  }


  load_irrig_cw <- setNames(lapply(conc_irrig_clearwater,
                                   function(x) x * inputs$clearwater.cmPerDay),
                            nm = sprintf("load_irrig_cw_%d", seq_len(length(conc_irrig_clearwater)))) %>%
    dplyr::bind_cols()

  if(n_conc[["conc_irrig_clearwater"]] == 1 & max(n_conc) > 1) {
    load_irrig_cw <- copy_column(load_irrig_cw,
                                 column = names(load_irrig_cw),
                                 number_of_new_columns = max(n_conc) - 1)
  }

  load_rain <- setNames(lapply(conc_rain,
                               function(x) x * inputs$rain.cmPerDay),
                        nm = sprintf("load_rain_%d", seq_len(length(conc_rain)))) %>%
    dplyr::bind_cols()

  if(n_conc[["conc_rain"]] == 1 & max(n_conc) > 1) {
    load_rain <- copy_column(load_rain,
                             column = names(load_rain),
                             number_of_new_columns = max(n_conc) - 1)
  }

  inf <- tibble::tibble(total_inflow.cmPerDay = inputs$total_inflow.cmPerDay)

  if(max(n_conc) > 1) {
    inf <- copy_column(inf,
                             column = names(inf),
                             number_of_new_columns = max(n_conc) - 1)
  }


  c_tops <- tibble::as_tibble((load_irrig_gw + load_irrig_cw + load_rain) / inf)
  c_tops[is.na(c_tops)] <- 0

  if(length(c_tops) == 1) {
   names(c_tops) <- "cTop"
  } else {
   names(c_tops) <- c("cTop", sprintf("cTop%d", seq_along(2:length(c_tops))+1))
  }

  c_tops_and_bots <- add_cBot_columns(c_tops)


  inputs_new <- inputs %>%
    dplyr::mutate(
      evapo_p_mean_cm = tidyr::replace_na(.data$evapo_p_mean_mm, 0) / 10,
      tAtm = dplyr::row_number(),
      Prec = (
        .data$rain.cmPerDay + .data$groundwater.cmPerDay + .data$clearwater.cmPerDay
      )
      ) %>%
    dplyr::rename(rSoil = .data$evapo_p_mean_cm) %>%
    dplyr::bind_cols(c_tops_and_bots) %>%
    dplyr::select(tidyselect::all_of(c("tAtm", "Prec", "rSoil")),
                  tidyselect::matches("c[B|T]"))

  kwb.hydrus1d::prepare_atmosphere_input(
    inputs = inputs_new,
    defaults = defaults
  )
}

