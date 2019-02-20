#' Eval layers by point
#'
#' @return
#' @export
#'
#' @examples
eval_pt <- function(x, layer, type = NULL, direction = NULL, ...) {
	if (is.null(x)) {
		stop('x must be provided. either data.table or sf point object.')
	}

	if (is.null(layer) || !("RasterLayer" %in% class(layer))) {
		stop('layer must be provided. expected type is raster.')
	}

	if (is.null(type) || is.null(direction)) {
		warning('missing type and/or direction. it is recommended to provide these for subsequent selection of camera trap locations.')
	}

	# if missing x and layer
	# if type isn't one of
	# if direction isn't one of
	# warn if no direction or no type

	nm <- deparse(substitute(layer))

	UseMethod('eval_pt', x)
}

#' @export
eval_pt.data.table <- function(x, layer, type, direction, coords) {
	if (length(coords) != 2) {
		stop('length of coords column names should be 2')
	}

	if (!all(sapply(DT[, .SD, .SDcols = coords], is.numeric))) {
		stop('coords provided must be numeric')
	}

	raster::extract(layer, x[, .SD, .SDcols = coords],
									na.rm = FALSE)
}

#' @export
eval_pt.sf <- function(x, layer, type, direction) {
	# if x isn't right type

	raster::extract(layer, sf::st_coordinates(x))
}


#' Eval layers by buffered
#'
#' @return
#' @export
#'
#' @examples
eval_buffer <- function(DT, layer, buffersize, type, direction) {
	# extract(buffer = buffersize)
}