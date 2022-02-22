#' Materials
#'
#' A dataset containing hydraulic soil characteristics from Hydrus1D GUI database
#'
#' @format A data frame with 12 rows and 7 variables:
#' \describe{
#'   \item{material_name}{Name of soil}
#'   \item{Qr}{Residual soil water content}
#'   \item{Qs}{Saturated soil water content}
#'   \item{Alpha}{Parameter a in the soil water retention function (L-1, here: cm)}
#'   \item{n}{Parameter n in the soil water retention function}
#'   \item{Ks}{Saturated hydraulic conductivity (unit: LT-1, here: cm/day)}
#'   \item{I}{Tortuosity parameter in the conductivity function (-)}
#' }
#' @source Hydrus1D GUI -> Soil Hydraulic Properties (Pre-Processing Menu, Water Flow Submenu)
#' @examples
#' materials
"materials"

