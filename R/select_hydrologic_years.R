#' Select hydrologic years
#'
#' @param atm atm as retrieved by \code{prepare_atmosphere_data}
#'
#' @return select hydrologic years
#' @export
#' @importFrom stringr str_detect
#' @examples
#' atm <- prepare_atmosphere_data()
#' atm_selected <- select_hydrologic_years(atm)
#' head(atm_selected)
#' tail(atm_selected)

select_hydrologic_years <- function(atm = prepare_atmosphere_data()) {

  which_matches <- function(pattern) {

    matches <- stringr::str_detect(atm$date, pattern)

    if (! any(matches)) {
      stop("Could not find pattern '", pattern, "' in column 'date'")
    }

    which(matches)
  }

  atm[min(which_matches("-05-01")):max(which_matches("-10-31")), ]
}
