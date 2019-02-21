#' Evaluate camera trap locations by point sampling layers
#'
#' Using the point locations generated manually or with `wildcam` functions [strat_sample()] and [make_grid()], sample raster layers to characterize and select camera trap locations and understand sampling bias
#'
#'
#' @inheritParams make_grid
#' @param layer raster layer.
#' @param type one of 'categorical', 'binary', 'ordinal', or 'real'. See Details.
#' @param direction one of 'positive', 'neutral', 'negative'. See Details.
#' @param ...
#'
#' @rdname eval_pt-methods
#' @aliases eval_pt
#'
#' @return
#' @export
#'
#' @seealso eval_buffer
#'
#' @examples
#' # Load data
#' data(points)
#' data(lc)
#'
#' # Evaluate each point with the land cover layer
#' #   type is categorical, and the direction is neutral
#' points$lc <- eval_pt(x = points, layer = lc, type = 'categorical', direction = 'neutral')
#'
#' plot(points)
eval_pt <- function(x, layer, type = NULL, direction = NULL, ...) {
	if (is.null(x)) {
		stop('x must be provided. either data.table or sf point object.')
	}

	if (is.null(layer) | !("RasterLayer" %in% class(layer))) {
		stop('layer must be provided. expected type is raster.')
	}

	if (is.null(type) | is.null(direction)) {
		warning('missing type and/or direction. it is recommended to provide these for subsequent selection of camera trap locations.')
	}

	checkls <- list(type, direction)
	if (sum(lengths(checkls)) != length(Filter(is.character, checkls))) {
		stop('type and direction must be of class character')
	}

	types <- c('categorical', 'binary', 'ordinal', 'real')
	directions <- c('positive', 'neutral', 'negative')

	# if type isn't one of
	# if direction isn't one of

	nm <- deparse(substitute(layer))

	UseMethod('eval_pt', x)
}

#' @export
#' @rdname eval_pt-methods
#' @aliases eval_pt, eval_pt-data.table-method
eval_pt.data.table <- function(x, layer, type, direction, coords) {
	if (length(coords) != 2) {
		stop('length of coords column names should be 2')
	}

	if (!all(vapply(DT[, .SD, .SDcols = coords], is.numeric, TRUE))) {
		stop('coords provided must be numeric')
	}

	set_eval_attr(raster::extract(layer, x[, .SD, .SDcols = coords],
																na.rm = FALSE),
								layer = nm, type = type, direction = direction)
}

#' @export
#' @rdname eval_pt-methods
#' @aliases eval_pt, eval_pt-sf-method
eval_pt.sf <- function(x, layer, type, direction) {
	if (!('geometry' %in% colnames(x))) {
		stop('geometry column not found in x')
	}

	if (!('sfc_POINT' %in% class(x$geometry))) {
		stop('class of geometry column must be sfc_POINT')
	}

	set_eval_attr(raster::extract(layer, sf::st_coordinates(x),
																na.rm = FALSE),
								layer = nm, type = type, direction = direction)
}


#' Eval layers by buffered
#'
#'
#' @inheritParams eval_pt
#' @param buffersize radius
#'
#' @return
#' @export
#'
#' @seealso eval_buffer
#'
#' @rdname eval_buffer-methods
#' @aliases eval_buffer
#'
#' @examples
eval_buffer <- function(x, layer, buffersize, type, direction, ...) {
	UseMethod('eval_buffer', x)


	# use extract(buffer = , fun = 'mean' if type else null if categorical eg)
	nm <- deparse(substitute(layer))

}


#' @export
#' @aliases eval_buffer, eval_buffer-data.table-method
#' @rdname eval_buffer-methods
eval_buffer.data.table <- function(x, layer, buffersize, type, direction, coords) {
	if (length(coords) != 2) {
		stop('length of coords column names should be 2')
	}

	if (!all(sapply(DT[, .SD, .SDcols = coords], is.numeric))) {
		stop('coords provided must be numeric')
	}

	raster::extract(layer, x[, .SD, .SDcols = coords],
									na.rm = FALSE, buffer = buffersize)
}

#' @export
#' @aliases eval_buffer, eval_buffer-sf-method
#' @rdname eval_buffer-methods
eval_buffer.sf <- function(x, layer, buffersize, type, direction) {
	# if x isn't right type
	set_eval_attr(raster::extract(layer, sf::st_coordinates(x)),
								layer = nm, type = type, direction = direction)
}


###
# reused data.table::setattr wrapper
set_eval_attr <- function(x, layer, type, direction) {
	# buffer size?
	# how to flex params + names added
	data.table::setattr(x,
											'wildcam',
											list(
												layer = layer,
												type = type,
												direction = direction
											))
}
