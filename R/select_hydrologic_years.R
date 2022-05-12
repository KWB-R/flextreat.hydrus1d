#' Select hydrologic years
#'
#' @param atm atm as retrieved by \code{prepare_atmosphere_data}
#'
#' @return select hydrologic years
#' @export
#' @importFrom stringr str_detect

select_hydrologic_years <- function(atm = prepare_atmosphere_data()) {

  start <- min(which(stringr::str_detect(atm$date, "-05-01")))
  end <- max(which(stringr::str_detect(atm$date, "-10-30")))

  atm[start:end,]


}
