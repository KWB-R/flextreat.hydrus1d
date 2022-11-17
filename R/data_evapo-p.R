#' DWD: Potential Evaporation, Daily
#'
#' Median daily potential evaporation for irrigation area (i.e. ~44km2), based
#' on 1x1km2 grids of DWD. Downloaded with \code{\link[kwb.dwd]{read_daily_data_over_shape}}
#' between 2017-01-01 and 2020-12-31
#'
#' @format A data.frame with 1461 rows and 10 variables:
#' \describe{
#'   \item{file}{name of downloaded grid file source}
#'   \item{date}{date}
#'   \item{year}{year}
#'   \item{month}{month}
#'   \item{day}{day}
#'   \item{mean}{spatially averaged, mean}
#'   \item{sd}{spatially averaged, standard deviation}
#'   \item{min}{spatially averaged, min}
#'   \item{max}{spatially averaged, max}
#'   \item{n_values}{number of grid-cells used for spatial averaging}
#' }
#' @examples
#' \dontrun{
#' ### Data download
#' remotes::install_github("kwb-r/kwb.dwd")
#' shape_file <- system.file("extdata/input-data/gis/Abwasserverregnungsgebiet.shp",
#' package = "flextreat.hydrus1d")
#'
#' # Only data of full months can currently be read!
#' evapo_p <- kwb.dwd::read_daily_data_over_shape(
#' file = shape_file,
#' variable = "evapo_p",
#' from = "201701",
#' to = "202012"
#' )}
#' head(flextreat.hydrus1d::evapo_p)
"evapo_p"
