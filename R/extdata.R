#' Example land cover
#'
#' A dataset containing land cover classes near Clearwater Lake, Manitoba.
#'
#' Land cover data are from 2020 Land Cover of Canada. Class index defined here
#' <https://drive.google.com/file/d/1TvOZdLO_N86HfsiJQtnE3BdfAFL1s0ux/view?usp=sharing>
#'
#' - 1 Temperate or sub-polar needleleaf forest/Forêt de conifères
#' sempervirente tempérée ou subpolaire
#' - 2 Sub-polar taiga needleleaf forest/ Forêt de conifères (taïga) subpolaire
#' - 5 Temperate or sub-polar broadleaf deciduous forest/ Forêt feuillue
#' tempérée ou subpolaire
#' - 6 Mixed forest/ Forêt mixte
#' - 8 Temperate or sub-polar shrubland/Arbustaie tempérée ou subpolaire
#' - 10 Temperate or sub-polar grassland/Prairie tempérée ou subpolaire
#' - 11 Sub-polar or polar shrubland-lichen-moss/Arbustaie à lichens et à
#' mousses polaire ou subpolaire
#' - 12 Sub-polar or polar grassland-lichen-moss/Prairie à lichens et à
#' mousses polaire ou subpolaire
#' - 13 Sub-polar or polar barren-lichen-moss/Lande à lichens et à mousses
#'  polaire ou subpolaire
#' - 14 Wetland/Milieu humide
#' - 15 Cropland/Terre cultivée
#' - 16 Barren Lands/Lande
#' - 17 Urban and built-up/Milieu urbain
#' - 18 Water/Eau
#' - 19 Snow and Ice/Neige et glace
#'
#' @format An external tif file to be read in with {terra} as a `SpatRaster`
#'   object (see [terra::rast()]). See the source below for details.
#'
#' @name clearwater_lake_land_cover
#' @source  <https://open.canada.ca/data/en/dataset/ee1580ab-a23d-4f86-a09b-79763677eb47>
#' @examples
#' library(terra)
#' clearwater_lc_path <- system.file("extdata",
#'   "clearwater_lake_land_cover.tif", package = "camtrapmonitoring")
#' clearwater_lake_land_cover <- rast(clearwater_lc_path)
#' plot(clearwater_lake_land_cover)
NULL



#' Example elevation
#'
#' A dataset containing elevation near Clearwater Lake, Manitoba.
#'
#' Elevation data are from the AWS using the {elevatr} package
#' ([elevatr::get_elev_raster()]).
#'
#' The units are meters.
#'
#' @format An external tif file to be read in with {terra} as a
#' `SpatRaster` object.
#'
#' @name clearwater_lake_elevation
#' @source  Hollister, J.W. (2022). elevatr: Access Elevation Data from
#' Various APIs. R package version 0.4.2.
#' <https://CRAN.R-project.org/package=elevatr/>
#' @examples
#' library(terra)
#' clearwater_elev_path <- system.file("extdata",
#'   "clearwater_lake_elevation.tif", package = "camtrapmonitoring")
#' clearwater_lake_elevation <- rast(clearwater_elev_path)
#' plot(clearwater_lake_elevation)
NULL

