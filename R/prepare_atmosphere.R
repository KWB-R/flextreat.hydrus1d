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
      rain_mm = tidyr::replace_na(.data$rain_mm, 0),
      groundwater.mmPerDay = tidyr::replace_na(.data$groundwater.mmPerDay, 0),
      clearwater.mmPerDay = tidyr::replace_na(.data$clearwater.mmPerDay, 0),
      total_inflow = rain_mm + clearwater.mmPerDay + groundwater.mmPerDay,
      load_rain = conc_rain * rain_mm,
      load_irrig_gw = conc_irrig_groundwater * groundwater.mmPerDay,
      load_irrig_cw = conc_irrig_clearwater * clearwater.mmPerDay,
      cTop = (load_rain + load_irrig_gw + load_irrig_cw) / total_inflow,
      evapo_p_mean_cm = tidyr::replace_na(.data$evapo_p_mean_mm, 0) / 10
    ) %>%
    dplyr::mutate(
      tAtm = dplyr::row_number(),
      Prec = (
        .data$rain_mm + .data$groundwater.mmPerDay + .data$clearwater.mmPerDay
      ) / 10,
      cTop = tidyr::replace_na(.data$cTop, 0)
    ) %>%
    dplyr::rename(rSoil = .data$evapo_p_mean_cm) %>%
    dplyr::select(tidyselect::all_of(c("tAtm", "Prec", "rSoil", "cTop")))

  kwb.hydrus1d::prepare_atmosphere_input(
    inputs = inputs,
    defaults = defaults
  )
}
