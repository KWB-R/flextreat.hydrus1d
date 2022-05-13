#' Prepare Atmosphere
#'
#' @param atm  atm as retrieved by \code{prepare_atmosphere_data}
#' @param defaults defaults for undefined parameters[kwb.hydrus1d::defaults_atmosphere()]
#' @return tibble with peoered
#' @export
#' @importFrom dplyr mutate rename select
#' @importFrom tidyselect all_of
#' @importFrom kwb.hydrus1d defaults_atmosphere
#' @examples
#'atm <- prepare_atmosphere_data()
#'atm_selected <- select_hydrologic_years(atm)
#'prepare_atmosphere(atm_selected)
prepare_atmosphere <- function(atm,
                               defaults = kwb.hydrus1d::defaults_atmosphere()) {

atm_headers <- c("tAtm", "Prec", "rSoil", "rRoot", "hCritA", "rB", "hB", "ht",
                 "tTop", "tBot", "Ampl", "cTop", "cBot", "RootDepth")

inputs <- atm %>%
  dplyr::mutate(tAtm = dplyr::row_number(),
                Prec = .data$rain_mm + .data$groundwater.mmPerDay + .data$clearwater.mmPerDay) %>%
  dplyr::rename(rSoil = .data$evapo_p_mean_mm) %>%
  dplyr::select(tidyselect::all_of(c("tAtm", "Prec", "rSoil")))


kwb.hydrus1d::prepare_atmosphere_input(inputs = inputs,
                                       defaults = defaults)

}

