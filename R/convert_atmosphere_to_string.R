#' Convert Atmosphere to String
#'
#' @param atm  atm as retrieved by \code{prepare_atmosphere_data}
#'
#' @return tibble with strings padded properly for Hydrus1D
#' @export
#' @importFrom stringr str_pad
#'
convert_atmosphere_to_string <- function(atm) {

headers_short <- "tAtm"
headers_long <- get_atmosphere_headers()[!get_atmosphere_headers() %in% headers_short]

atm[headers_short] <- (lapply(atm[headers_long],
                             function(x) stringr::str_pad(x, 11, "left")))
atm[headers_long] <- (lapply(atm[headers_long],
                             function(x) stringr::str_pad(x, 12, "left")))

atm
}
