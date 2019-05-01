#' Evaluate camera trap locations by point sampling layers
#'
#' Using the point locations generated manually or with `wildcam` functions [strat_sample()] and [grid_ct()], sample raster layers to characterize and select camera trap locations, and quantify potential sampling bias.
#'
#' Type is used to define the data type of the sampled raster layer:
#'
#' * 'categorical' - one of a limited and usually fixed number of possible values (e.g.: landcover. 1, 2, 3 indicating 'wetland', 'forest', 'agriculture').
#' * 'binary' - also boolean. TRUE/FALSE, presence/absence. (e.g.: raster of wetlands).
#' * 'ordinal' - ranked or ordered data. (e.g.: animal density high/medium/low).
#' * 'real' - real number variable either interval or ratio scale. (e.g.: digital elevation model).
#'
#' Direction is used to define the user's priority or preference for this layer. For example, 'positive' direction when evaluating an elevation layer would indicate that the sampled camera trap locations are preferably in areas of high elevation. This attribute is later used by [select_ct()] for ranking and selecting camera trap locations.
#'
#'
#' @inheritParams grid_ct
#' @param layer raster layer.
#' @param type one of 'categorical', 'binary', 'ordinal', or 'real'. See Details.
#' @param direction one of 'positive', 'neutral', 'negative'. See Details.
#'
#' @return
#' @export
#'
#' @family eval
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
#' plot(points["lc"])
eval_pt <-
	function(x,
					 layer,
					 type = NULL,
					 direction = NULL,
					 coords = NULL) {
		if (is.null(x)) {
			stop('x must be provided. either data.table or sf point object.')
		}

		if (is.null(layer) | !inherits(layer, 'Raster')) {
			stop('layer must be provided. expected type is raster.')
		}

		if (is.null(type) | is.null(direction)) {
			warning(
				'missing type and/or direction. it is recommended to provide these for subsequent selection of camera trap locations.'
			)
		}

		checkls <- list(type, direction)
		if (sum(lengths(checkls)) != length(Filter(is.character, checkls))) {
			stop('type and direction must be of class character')
		}

		types <- c('categorical', 'binary', 'ordinal', 'real')
		directions <- c('positive', 'neutral', 'negative')

		if (!is.null(type)) {
			if (!(type %in% types)) {
				stop('type must be one of ', paste(types, collapse = ', '))
			}
		}

		if (!is.null(direction)) {
			if (!(direction %in% directions)) {
				stop('direction must be one of ', paste(direction, collapse = ', '))
			}
		}

		eval_pt_(x, layer, type, direction, coords)
	}

#' @export
#' @describeIn eval_pt
eval_pt_ <- 	function(x,
											layer,
											type = NULL,
											direction = NULL,
											coords = NULL) {
	UseMethod('eval_pt_')
}


#' @export
#' @describeIn eval_pt
eval_pt_.data.table <-
	function(x,
					 layer,
					 type = NULL,
					 direction = NULL,
					 coords = NULL) {
		if (is.null(coords)) {
			stop('coords must be provided if x is a data.table')
		}

		if (length(coords) != 2) {
			stop('coords must be a character vector of length 2')
		}

		if (any(!(coords %in% colnames(x)))) {
			stop('geometry column not found in x')
		}

		if (!all(vapply(x[, .SD, .SDcols = coords], is.numeric, TRUE))) {
			stop('coords provided must be numeric')
		}

		set_eval_attr(
			raster::extract(layer, x[, .SD, .SDcols = coords],
											na.rm = FALSE),
			layer = deparse(substitute(layer)),
			type = type,
			direction = direction
		)[]
	}

#' @export
#' @describeIn eval_pt
eval_pt_.sf <-
	function(x,
					 layer,
					 type = NULL,
					 direction = NULL,
					 coords = NULL) {
		if (!('geometry' %in% colnames(x))) {
			stop('geometry column not found in x')
		}

		if (!inherits(x$geometry, 'sfc_POINT')) {
			stop('class of geometry column must be sfc_POINT')
		}

		set_eval_attr(
			raster::extract(layer, sf::st_coordinates(x),
											na.rm = FALSE),
			layer = deparse(substitute(layer)),
			type = type,
			direction = direction
		)
	}


#' Evaluate camera trap locations by buffered sampling of layers
#'
#' Using the buffered points locations generated manually or with `wildcam` functions [strat_sample()] and [grid_ct()], sample raster layers to characterize and select camera trap locations, and quantify potential sampling bias.
#'
#' Type is used to define the data type of the sampled raster layer:
#'
#' * 'categorical' - one of a limited and usually fixed number of possible values (e.g.: landcover. 1, 2, 3 indicating 'wetland', 'forest', 'agriculture').
#' * 'binary' - also boolean. TRUE/FALSE, presence/absence. (e.g.: raster of wetlands).
#' * 'ordinal' - ranked or ordered data. (e.g.: animal density high/medium/low).
#' * 'real' - real number variable either interval or ratio scale. (e.g.: digital elevation model).
#'
#' The function used to summarize the buffer regions around each point in `x` depends on the data `type` defined. 'categorical' data should likely be transformed into individual binary layers with [make_binary()] so `mean` can be used to determine proportions in a buffer region.
#'
#' Direction is used to define the user's priority or preference for this layer. For example, 'positive' direction when evaluating an elevation layer would indicate that the sampled camera trap locations are preferably in areas of high elevation. This attribute is later used by [select_ct()] for ranking and selecting camera trap locations.
#'
#' @inheritParams eval_pt
#' @param buffersize radius of buffer around each point.
#'
#' @return
#' @export
#'
#' @family eval
#'
#' @examples
#' # Load data
#' data(points)
#' data(wetland)
#'
#' # Evaluate each point with the wetland layer
#' points$wetland <- eval_buffer(points, wetland, buffersize = 150,
#'       type = 'binary', direction = 'positive')
#'
#' plot(points["wetland"])
eval_buffer <-
	function(x,
					 layer,
					 buffersize,
					 type,
					 direction,
					 coords = NULL) {
	if (is.null(x)) {
		stop('x must be provided. either data.table or sf point object.')
	}

	if (is.null(layer) | !inherits(layer, 'Raster')) {
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

	if (!is.null(type)) {
		if (!(type %in% types)) {
			stop('type must be one of ', paste(types, collapse = ', '))
		}
	}

	if (!is.null(direction)) {
		if (!(direction %in% directions)) {
			stop('direction must be one of ', paste(direction, collapse = ', '))
		}
	}

	if (any(buffersize < raster::res(layer))) {
		warning("buffersize is less than the layer's resolution")
	}

	# TODO: add crs = crs(layer)

	eval_buffer_(x, layer, buffersize, type, direction, coords)
}

#' @export
#' @describeIn eval_buffer
eval_buffer_ <- function(x,
												 layer,
												 buffersize,
												 type,
												 direction,
												 coords = NULL) {
	UseMethod('eval_buffer_')
}



#' @export
#' @describeIn eval_buffer
eval_buffer_.data.table <-
	function(x,
					 layer,
					 buffersize,
					 type,
					 direction,
					 coords = NULL) {
		if (is.null(coords)) {
			stop('coords must be provided if x is a data.table')
		}

		if (length(coords) != 2) {
			stop('coords must be a character vector of length 2')
		}

		if (any(!(coords %in% colnames(x)))) {
			stop('geometry column not found in x')
		}

		if (!all(vapply(x[, .SD, .SDcols = coords], is.numeric, TRUE))) {
			stop('coords provided must be numeric')
		}

		if (!is.null(type)) {
			if (type %in% c('binary', 'real')) {
				bufferfun <- mean
			} else if (type %in% c('categorical', 'ordinal')) {
				bufferfun <- NULL
				warning('type provided is either categorical or ordinal, cannot summarize in buffer, returning frequency table')
			} else {
				stop("type must be one of 'categorical', 'binary', 'ordinal', 'real'")
			}
		} else {
			bufferfun <- NULL
		}
		# how to summarize buffers with ordinal/categorical

		set_eval_attr(
			raster::extract(layer,
											x[, .SD, .SDcols = coords],
											buffer = buffersize,
											fun = bufferfun),
			layer = deparse(substitute(layer)),
			type = type,
			direction = direction
		)[]
	}

#' @export
#' @describeIn eval_buffer
eval_buffer_.sf <-
	function(x,
					 layer,
					 buffersize,
					 type,
					 direction,
					 coords = NULL) {
		if (!('geometry' %in% colnames(x))) {
			stop('geometry column not found in x')
		}

		if (!inherits(x$geometry, 'sfc_POINT')) {
			stop('class of geometry column must be sfc_POINT')
		}

		if (!is.null(coords)) {
			warning('coords provided are ignored because x is an sf object')
		}

		if (!is.null(type)) {
			if (type %in% c('binary', 'real')) {
				bufferfun <- mean
			} else if (type %in% c('categorical', 'ordinal')) {
				bufferfun <- NULL
				warning('type provided is either categorical or ordinal, cannot summarize in buffer, returning frequency table')
			} else {
				stop("type must be one of 'categorical', 'binary', 'ordinal', 'real'")
			}
		} else {
			bufferfun <- NULL
		}
		# how to summarize buffers with ordinal/categorical


		set_eval_attr(
			raster::extract(
				layer,
				sf::st_coordinates(x),
				buffer = buffersize,
				fun = bufferfun
			),
			layer = deparse(substitute(layer)),
			type = type,
			direction = direction
		)
	}

#' Evaluate distance-to
#'
#' Evaluates locations in x by measuring the distance to the nearest feature in y.
#'
#' To avoid the large overhead of creating distance to rasters for small/medium number of sample points, this vector-based distance to determines the nearest feature (y) to each x then calculates the distance between each pair.
#'
#' @inheritParams eval_pt
#' @param y object of class sfg, sfc or sf.
#'
#' @return Vector of distances between x and the nearest feature in y.
#'
#' @family eval
#' @export
#'
#' @examples
#' # sf objects
#' data(water)
#' data(points)
#'
#' points$distWater <- eval_dist(points, water)
#'
#' # data.table objects
#' data(DT)
#' alloc.col(DT)
#'
#' DT[, distWater := eval_dist(.SD, water, coords = c('X', 'Y', crs = sf::st_crs(water)))]
eval_dist <- function(x, y, coords = NULL, crs = NULL) {
	if (is.null(x) | is.null(y)) {
		stop('please provide both x and y')
	}

	# check types of x and y
	eval_dist_(x = x, y = y, coords = coords, crs = crs)
}

#' @export
#' @describeIn eval_dist
eval_dist_ <- function(x, y, coords = NULL, crs = NULL) {
	UseMethod('eval_dist_', x)
}

#' @export
#' @describeIn eval_dist
eval_dist_.sf <- function(x, y, coords = NULL, crs = NULL) {
	sf::st_distance(x, y[sf::st_nearest_feature(x, y), ],
									by_element = TRUE)
}

#' @export
#' @describeIn eval_dist
eval_dist_.data.table <-
	function(x,
					 y = NULL,
					 coords = NULL,
					 crs = NULL) {
		xsf <- sf::st_as_sf(x, coords = coords, crs = crs)
		sf::st_distance(xsf,
										y[sf::st_nearest_feature(xsf, y), ],
										by_element = TRUE)
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
