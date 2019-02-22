#' Scale in region of interest
#'
#' Scale a layer in a region of interest to optionally compare locations with [eval_pt()] and [eval_buffer()], and select locations with [select_ct()] based off of relative values instead of absolute values.
#'
#' @inheritParams eval_pt
#' @param roi any object which can be passed to extent including `sf`, `Spatial`, `Raster` objects and 2x2 matrices.
#'
#' @return `Raster` layer, cropped to extent of provided 'roi', and center scaled.
#'
#' @seealso [raster::scale()]
#'
#' @export
#'
#' @examples
#' # Load data
#' data(densitygrid)
#' data(dem)
#'
#' # Scale elevation layer in extent of density grid
#' scale_roi(layer = dem, roi = densitygrid)
scale_roi <- function(layer, roi) {
	if (missing(layer) | !("RasterLayer" %in% class(layer))) {
		stop('layer must be provided and expected type is raster.')
	}

	raster::scale(raster::crop(layer, roi))
}