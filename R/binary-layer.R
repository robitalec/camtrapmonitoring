#' Binary layer
#'
#' Make a binary raster layer from input x.
#'
#' Find all pixels matching the 'value' provided given the 'fun' and return a binary raster.
#'
#' @inheritParams eval_pt
#' @param value numeric value in x. see Details.
#' @param fun character indicating which function to use to compare layer to value. One of 'equals', 'gt', 'gte', 'lt', 'lte' or 'in'. Default: 'equals'.
#'
#' @return
#' A binary raster with two values: `TRUE` if pixel matches 'value' provided and `FALSE` if pixel does not match 'value' provided.
#'
#' 'value' may only be length 1 if 'fun' is one of: 'equals', 'gt', 'gte', 'lt', 'lte'.
#'
#' 'value' may be greater than length 1 if 'fun' is: 'in'.
#'
#' @export
#'
#' @examples
#' library(terra)
#' clearwater_lc_path <- system.file("extdata", "clearwater_lake_land_cover.tif", package = "wildcam")
#' clearwater_lake_land_cover <- rast(clearwater_lc_path)
#'
#' bin <- binary_layer(clearwater_lake_land_cover, 18, fun = 'equals')
#'
#' plot(bin)
#'
#' # fun = 'in'
#' bin <- binary_layer(clearwater_lake_land_cover, c(1, 2), fun = 'in')
#'
#' plot(bin)
binary_layer <- function(x, value, fun = 'equals', layer = 1) {
	if (missing(x)) {
		stop('x must be provided.')
	}

	if (missing(value)) {
		stop('value must be provided.')
	}

	if (missing(fun)) {
		if (length(value) == 1) {
			message('argument "fun" is missing, defaulting to "equals"')
		} else if (length(value) > 1) {
			message('argument "fun" is missing, defaulting to "in"')
		}
	}

	if (!inherits(x, 'SpatRaster')) {
		stop('x must be a SpatRaster.')
	}

	if (!inherits(value, 'numeric')) {
		stop('value must be a numeric.')
	}

	if (length(value) == 1) {
		if (fun == 'equals') {
			return(x == value)
		} else if (fun == 'gt') {
			return(x > value)
		} else if (fun == 'gte') {
			return(x >= value)
		} else if (fun == 'lt') {
			return(x < value)
		} else if (fun == 'lte') {
			return(x <= value)
		} else {
			stop('fun must be "equals", "gt", "gte", "lt", or "lte" if length of value is 1')
		}
	} else if (length(value) > 1) {
		if (fun == 'in' | fun == 'equals') {
			return(terra::`%in%`(x, value))
		} else {
			stop('fun must be "in" if length of value is > 1')
		}
	}
}
