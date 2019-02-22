#' Scale in region of interest
#'
#' Scale a layer in a region of interest to optionally compare locations with [eval_pt()] and [eval_buffer()], and select locations with [select_ct()] based off of relative values instead of absolute values.
#'
#' @param layer
#' @param roi sf
#'
#' @return
#' @export
#'
#' @examples
scale_roi <- function(layer, roi) {
	raster::scale(raster::crop(layer, roi))
}