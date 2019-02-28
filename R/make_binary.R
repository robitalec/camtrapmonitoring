#' Binary rasters
#'
#' Make a binary raster from input 'layer'.
#'
#' @param layer
#' @param value
#'
#' @return
#'
#' A binary raster layer with two values:
#'
#' @export
#'
#' @examples
make_binary <- function(layer, value) {
	if (missing(layer)) {
		stop('layer must be provided.')
	}

	if (missing(value)) {
		stop('value must be provided.')
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

	return(is.na(raster::mask(layer, layer, maskvalue = value)))
}
