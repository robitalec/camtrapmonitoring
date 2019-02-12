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
#' The projection for this layer is UTM 14N WGS84. The proj4string for this layer is "+proj=utm +zone=14 +datum=WGS84 +units=m +no_defs", EPSG code 32614.
#'
#' @source [Manitoba Land Initiative](http://mli2.gov.mb.ca/roads_hwys/index.html)
#'
#' @examples
#' data(water)
#' plot(water)
"water"