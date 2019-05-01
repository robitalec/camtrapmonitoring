#' Binary rasters
#'
#' Make a binary raster from input 'layer'.
#'
#' Find all pixels matching the 'value' provided and return binary raster.
#'
#' @inheritParams eval_pt
#' @param value numeric value in 'layer'. expected length is 1.
#'
#' @return
#'
#' A binary raster layer with two values: `TRUE` if pixel matches 'value' provided and `FALSE` if pixel does not match 'value' provided.
#'
#' @export
#'
#' @examples
#' data(lc)
#'
#' bin <- binary_layer(lc, 212)
#'
#' image(bin)
binary_layer <- function(layer, value, fun) {
	if (missing(layer)) {
		stop('layer must be provided.')
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

	if (!inherits(layer, 'Raster')) {
		stop('layer must be a raster.')
	}

	if (!inherits(value, 'numeric')) {
		stop('value must be a numeric.')
	}

	if (length(value) != 1) {
		stop('value must be of length one.')
	}


}
