#' Scale in region of interest
#'
#' Helper function to scale a target layer in a region of interest.
#' Can be used to compare locations with [eval_pt()] and [eval_buffer()],
#' and select locations based off of relative values instead of absolute values.
#'
#' @inheritParams eval_pt
#' @param region object which can be passed to [terra::ext()] including `sf`,
#' `Spatial`, `Raster` objects and 2x2 matrices.
#' @param center see [terra::scale()]
#' @param scale see [terra::scale()]
#'
#' @return `SpatRaster` layer, cropped to extent of provided region, and scaled.
#'
#' @seealso [terra::scale()]
#'
#' @export
#'
#' @examples
#' # Load packages
#' library(terra)
#'
#' # Load data
#' data("clearwater_lake_hydro")
#' clearwater_elev_path <- system.file(
#'   "extdata", "clearwater_lake_elevation.tif", package = "camtrapmonitoring")
#' clearwater_lake_elevation <- rast(clearwater_elev_path)
#'
#' # Region of interest around Clearwater lake
#' roi <- clearwater_lake_hydro[4,]
#'
#' # Scale elevation in extent of density grid
#' elev_scaled <- scale_layer(target = clearwater_lake_elevation, region = roi)
scale_layer <- function(target, region, center = TRUE, scale = TRUE) {
	if (missing(target) | is.null(target)) {
		stop('target must be provided')
	}
	if (!inherits(target, 'SpatRaster')) {
		stop('target must be a SpatRaster')
	}

	if (missing(region) | is.null(region)) {
		stop('region must be provided.')
	}

	# add check for compatible with extent

	return(
		terra::scale(terra::crop(target, region), center, scale)
	)
}