#' Example roads
#'
#' A dataset containing roads nearby Clearwater Lake, Manitoba.
#'
#' Roads are from the Canadian National Road Network (NRN) with all corresponding fields.
#'
#' @format An `sf` object with 172 roads and 48 variables. See the source below for details.
#'
#' @source <https://open.canada.ca/data/en/dataset/3d282116-e556-400c-9306-ca1a3cada77f>
#' @examples
#' data(clearwater_lake_roads)
#' plot(clearwater_lake_roads)
"clearwater_lake_roads"


#' Example hydrology features
#'
#' A dataset containing hydrology features (in this case, major lakes)
#' nearby Clearwater Lake, Manitoba.
#'
#' Hydrology features are from Open Street Map, downloaded using the {osmdata}
#' package.
#'
#' @format An `sf` object with 5 features and 8 fields. See the source below for details.
#'
#' @source  Mark Padgham, Bob Rudis, Robin Lovelace, Maëlle Salmon (2017). “osmdata.” _Journal of Open Source Software_, *2*(14), 305. doi:10.21105/joss.00305 <https://doi.org/10.21105/joss.00305>, <https://joss.theoj.org/papers/10.21105/joss.00305>.
#' @examples
#' data(clearwater_lake_hydro)
#' plot(clearwater_lake_hydro)
"clearwater_lake_hydro"
