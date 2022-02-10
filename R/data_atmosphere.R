#' Atmosphere
#'
#' A dummy dataset containing minimum atmospheric data
#'
#' @format A data frame with 12 rows and 7 variables:
#' \describe{
#'   \item{tAtm}{Time for which the i-th data record is provided (T)}
#'   \item{Prec}{Precipitation rate (LT-1) (in absolute value)}
#'   \item{rSoil}{Potential evaporation rate (LT-1) (in absolute value). rSoil(i) is
#'   interpreted as KodTop when a time variable Dirichlet or Neumann boundary
#'   condition is specified.}
#'   \item{rRoot}{Potential transpiration rate (LT-1) (in absolute value).}
#'   \item{hCritA}{Absolute value of the minimum allowed pressure head at the soil
#'   surface (L).}
#' }
#' @source \url{https://raw.githubusercontent.com/phydrus/phydrus/dev/examples/phydrus_paper/data/atmosphere.csv/}
#' @examples
#' head(atmosphere)
"atmosphere"
