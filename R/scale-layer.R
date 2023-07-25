#' Scale in region of interest
#'
#' Scale a layer in a region of interest to optionally compare locations
#' with [eval_pt()] and [eval_buffer()], and select locations based off of
#' relative values instead of absolute values.
#'
#' @inheritParams eval_pt
#' @param roi any object which can be passed to extent including `sf`,
#' `Spatial`, `Raster` objects and 2x2 matrices.
#' @param center see terra::scale
#' @param scale see terra::scale
#'
#' @return `SpatRaster` layer, cropped to extent of provided 'roi', and scaled.
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
#' clearwater_elev_path <- system.file(
#'   "extdata", "clearwater_lake_elevation.tif", package = "camtrapmonitoring")
#' clearwater_lake_elevation <- rast(clearwater_elev_path)
#'
#' # Region of interest: Clearwater lake area
#' roi <- ext(clearwater_lake_elevation)
#'
#' # Scale elevation in extent of density grid
#' elev_scaled <- scale_layer(clearwater_lake_elevation, roi)
#'
#' # Plot
#' plot(elev_scaled)
scale_layer <- function(x, roi, center = TRUE, scale = TRUE) {
	if (missing(x) | is.null(x) | !inherits(x, 'SpatRaster')) {
		stop('x must be provided and expected type is SpatRaster.')
	}

	if (missing(roi) | is.null(roi)) {
		stop('roi must be provided.')
	}

	# add check for compatible with extent

	return(
		terra::scale(terra::crop(x, roi), center, scale)
	)
}