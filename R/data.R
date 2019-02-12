#' Water polygons
#'
#' A dataset containing three polygons representing three lakes in Manitoba.
#'
#' @format An `sf` object with 3 polygons and four variables:
#'
#' * ID: individual identifier, numeric type
#' * PERIMETER: perimeter of the water body
#' * AREA: area of the water body
#' * geometry: `sfc` simple feature column
#'
#' @source [Manitoba Land Initiative](http://mli2.gov.mb.ca/roads_hwys/index.html)
#'
#' @examples
#' data(water)
#' plot(water)
"water"