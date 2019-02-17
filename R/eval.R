#' Eval layers by point
#'
#' @return
#' @export
#'
#' @examples
eval_pt <- function(x, layer, type, direction, ...) {
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
	extract(buffer = buffersize)
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