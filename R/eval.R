#' Eval layers by point
#'
#' @return
#' @export
#'
#' @examples
eval_pt <- function(x, layer, type, direction, ...) {
	if (is.null(x)) {
		stop('x must be provided. either data.table or sf point object.')
	}

	if (is.null(layer) | !("raster" %in% class(layer))) {
		stop('layer must be provided. expected type is raster.')
	}

	if (is.null(type) | is.null(direction)) {
		warning('missing type and/or direction. it is recommended to provide these for subsequent selection of camera trap locations.')
	}

	# if type isn't one of
	# if direction isn't one of

	nm <- deparse(substitute(layer))

	UseMethod('eval_pt', x)
}

#' @export
eval_pt.data.table <- function(x, layer, type, direction, coords) {
	if (length(coords) != 2) {
		stop('length of coords column names should be 2')
	}

	# if type numeric coords
	# if missing x and layer
	# warn if no type and direction

	raster::extract(layer, x[, data.table::.SD, .SDcols = coords],
									na.rm = FALSE)
}

#' @export
eval_pt.sf <- function(x, layer, type, direction) {
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

#' Eval layers by point
#'
#' @return
#' @export
#'
#' @examples
eval_grid <- function(DT, layer, grid, type, direction) {
	# build grid or expect grid?
}