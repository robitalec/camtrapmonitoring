#' Binary layer
#'
#' Helper function to make a binary raster layer from input target.
#'
#' Find all pixels given the function ('fun') matching the value ('value')
#' and return a binary raster.
#'
#' @inheritParams eval_pt
#' @param fun character indicating which function to use to compare layer to
#' value. One of 'equals', 'gt', 'gte', 'lt', 'lte' or 'in'. Default: 'equals'.
#' @param value numeric value in target. see Details.
#'
#' @return
#' A binary raster (`SpatRaster`) with two values:
#' - `TRUE` if pixel matches 'value' provided given the provided function
#' - `FALSE` if pixel does not match 'value' provided given the provided function
#'
#' 'value' may only be length 1 if 'fun' is one of: 'equals', 'gt', 'gte',
#' 'lt', 'lte'.
#'
#' 'value' may be greater than length 1 only if 'fun' is: 'in'.
#'
#' @export
#'
#' @examples
#' library(terra)
#' clearwater_lc_path <- system.file("extdata",
#'   "clearwater_lake_land_cover.tif", package = "camtrapmonitoring")
#' clearwater_lake_land_cover <- rast(clearwater_lc_path)
#'
#' bin <- binary_layer(target = clearwater_lake_land_cover,
#'   fun = 'equals', value = 18)
#'
#' plot(bin)
#'
#' # fun = 'in'
#' bin <- binary_layer(target = clearwater_lake_land_cover,
#'   fun = 'in', value = c(1, 2))
#'
#' plot(bin)
binary_layer <- function(target, fun = 'equals', value, layer = 1) {
	if (missing(target)) {
		stop('target must be provided.')
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

	if (!inherits(target, 'SpatRaster')) {
		stop('target must be a SpatRaster.')
	}

	if (!inherits(value, 'numeric')) {
		stop('value must be a numeric.')
	}

	if (length(value) == 1) {
		if (fun == 'equals') {
			return(target == value)
		} else if (fun == 'gt') {
			return(target > value)
		} else if (fun == 'gte') {
			return(target >= value)
		} else if (fun == 'lt') {
			return(target < value)
		} else if (fun == 'lte') {
			return(target <= value)
		} else {
			stop('fun must be "equals", "gt", "gte", "lt", or "lte" if length of value is 1')
		}
	} else if (length(value) > 1) {
		if (fun == 'in' | fun == 'equals') {
			return(terra::`%in%`(target, value))
		} else {
			stop('fun must be "in" if length of value is > 1')
		}
	}
}
