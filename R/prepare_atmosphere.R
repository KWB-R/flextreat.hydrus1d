#' Prepare Atmosphere
#'
#' @param atm  atm as retrieved by \code{prepare_atmosphere_data}
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
prepare_atmosphere <- function(atm,
                               defaults = kwb.hydrus1d::defaults_atmosphere()) {


inputs <- atm %>%
  dplyr::mutate(rain_mm = tidyr::replace_na(.data$rain_mm, 0),
                groundwater.mmPerDay = tidyr::replace_na(.data$groundwater.mmPerDay, 0),
                clearwater.mmPerDay = tidyr::replace_na(.data$clearwater.mmPerDay, 0),
                evapo_p_mean_mm = tidyr::replace_na(.data$evapo_p_mean_mm, 0)) %>%
  dplyr::mutate(tAtm = dplyr::row_number(),
                Prec = .data$rain_mm + .data$groundwater.mmPerDay + .data$clearwater.mmPerDay) %>%
  dplyr::rename(rSoil = .data$evapo_p_mean_mm) %>%
  dplyr::select(tidyselect::all_of(c("tAtm", "Prec", "rSoil")))


kwb.hydrus1d::prepare_atmosphere_input(inputs = inputs,
                                       defaults = defaults)

}

