#' Evaluate camera trap locations by point sampling layers
#'
#' Using the point locations generated manually or with `camtrapmonitoring`
#' functions [sample_ct()] and [grid_ct()], sample raster layers to
#' characterize and select camera trap locations, and quantify potential
#' sampling bias.
#'
#'
#' @param x SpatRaster layer (terra package)
#' @param y spatial feature object
#' @param layer default 1, see terra::extract
#'
#' @return vector of values from x matching locations in y
#' @export
#'
#' @family eval
#'
#' @examples
#' data("clearwater_lake_density")
#' clearwater_lake_land_cover <- rast(system.file('extdata',
#'   'clearwater_lake_land_cover.tif', package = 'camtrapmonitoring'))
#'
#' # Sample points
#' pts <- sample_ct(clearwater_lake_density, 1, type = 'random')
#'
#' # Make grid with queen's case
#' queen <- grid_ct(pts, case = 'queen', distance = 100)
#'
#' # Evaluate each point with the land cover layer
#' queen$lc <- eval_pt(x = clearwater_lake_land_cover, y = queen)
#'
#' plot(queen["lc"])
eval_pt <-
	function(x,
					 y,
					 layer = 1) {
		if (missing(x) || is.null(x) || !inherits(x, 'SpatRaster')) {
			stop('x must be provided. expected type is SpatRaster.')
		}
		if (missing(y) || is.null(y)) {
			stop('y must be provided.')
		}

		# if (is.null(type) || is.null(direction)) {
		# 	warning(
		# 		'missing type and/or direction. it is recommended to provide
		#these for subsequent selection of camera trap locations.'
		# 	)
		# }

		# checkls <- list(type, direction)
		# if (sum(lengths(checkls)) != length(Filter(is.character, checkls))) {
		# 	stop('type and direction must be of class character')
		# }

		# check_type(type)
		# check_direction(direction)

		# type = NULL,
		# direction = NULL,
		# coords = NULL) {
		stopifnot('y is not of class sf' = inherits(y, 'sf'))
		stopifnot('y is not of geometry type POINT' =
								sf::st_geometry_type(y, FALSE) == 'POINT')

		terra::extract(
			x = x,
			y = y,
			layer = layer,
			na.rm = FALSE,
			simple = TRUE,
			ID = FALSE
		)[[layer]]
		# layer = deparse(substitute(layer)),
		# type = type,
		# direction = direction
		# )
	}


#' Evaluate camera trap locations by buffered sampling of layers
#'
#' Using the buffered points locations generated manually or with
#' `camtrapmonitoring` functions [sample_ct()] and [grid_ct()], sample
#'  raster layers to characterize and select camera trap locations,
#'  and quantify potential sampling bias.
#'
#'
#' @inheritParams eval_pt
#' @param buffer_size radius of buffer around each point
#' @param buffer_fun function for summarizing buffer region, default mean
#'
#' @return vector of values from x matching buffered locations in y
#' @export
#'
#' @family eval
#'
#' @examples
#' library(terra)
#' data("clearwater_lake_density")
#' clearwater_lake_elevation <- rast(system.file('extdata',
#'   'clearwater_lake_elevation.tif', package = 'camtrapmonitoring'))
#'
#' # Sample points
#' pts <- sample_ct(clearwater_lake_density, 1, type = 'random')
#'
#' # Make grid with queen's case
#' queen <- grid_ct(pts, case = 'queen', distance = 100)
#'
#' # Evaluate each point with the land cover layer
#' queen$elev <- eval_buffer(
#'   x = clearwater_lake_elevation, y = queen, buffer_size = 150)
#'
#' plot(queen["elev"])
eval_buffer <-
	function(x,
					 y,
					 buffer_size,
					 buffer_fun = mean,
					 layer = 1) {

		if (missing(x) || is.null(x) || !inherits(x, 'SpatRaster')) {
			stop('x must be provided. expected type is SpatRaster.')
		}
		if (missing(y) || is.null(y)) {
			stop('y must be provided.')
		}

		# if (is.null(type) || is.null(direction)) {
		# 	warning('missing type and/or direction. it is recommended to
		# provide these for subsequent selection of camera trap locations.')
		# }
		#
		# checkls <- list(type, direction)
		# if (sum(lengths(checkls)) != length(Filter(is.character, checkls))) {
		# 	stop('type and direction must be of class character')
		# }
		#
		# check_type(type)
		# check_direction(direction)

		if (any(buffer_size < terra::res(x))) {
			warning("buffer_size is less than the x's resolution")
		}

		# TODO: add crs = crs(layer)

		# coords = NULL) {
		stopifnot('y is not of class sf' = inherits(y, 'sf'))
		stopifnot('y is not of geometry type POINT' =
								sf::st_geometry_type(y, FALSE) == 'POINT')


		# if (!is.null(coords)) {
		# 	warning('coords provided are ignored because x is an sf object')
		# }

		# if (!is.null(type)) {
		# 	if (type %in% c('binary', 'real')) {
		# 		bufferfun <- mean
		# 	} else if (type %in% c('categorical', 'ordinal')) {
		# 		bufferfun <- NULL
		# 		warning('type provided is either categorical or ordinal, cannot
		#summarize in buffer, returning frequency table')
		# 	} else {
		# 		stop("type must be one of 'categorical', 'binary', 'ordinal', 'real'")
		# 	}
		# } else {
		# 	bufferfun <- NULL
		# }
		# how to summarize buffers with ordinal/categorical

		terra::extract(
			x = x,
			y = st_buffer(y, dist = buffer_size),
			layer = layer,
			na.rm = FALSE,
			fun = buffer_fun,
			ID = FALSE
		)[[layer]]
	}



#' Evaluate distance-to
#'
#' Evaluates locations in x by measuring the distance to the nearest feature
#' in layer.
#'
#' To avoid the large overhead of creating distance to rasters for small/medium
#'  number of sample points, this vector-based distance to determines the
#'  nearest feature (layer) to each x then calculates the distance between
#'  each pair.
#'
#' @inheritParams eval_pt
#' @param measure measure type see geodist::geodist for details
#'
#' @return vector of distances between x and y
#'
#' Note attributes are returned like by `eval_pt` and `eval_buffer`. The
#' `type` attribute for distance to a feature (layer) is "real" and the
#' `direction` is left for the user to provide.
#'
#' @family eval
#' @export
#'
#' @examples
#' library(terra)
#' data("clearwater_lake_density")
#' data("clearwater_lake_wetlands")
#'
#' # Sample points
#' pts <- sample_ct(clearwater_lake_density, 1, type = 'random')
#'
#' # Make grid with queen's case
#' queen <- grid_ct(pts, case = 'queen', distance = 100)
#'
#' # Evaluate each point with the land cover layer
#' queen$dist_wetland <- eval_dist(x = clearwater_lake_wetlands, y = queen)
#'
#' # Plot
#' plot(queen["dist_wetland"])
eval_dist <-
	function(x,
					 y,
					 measure = NULL,
					 direction = NULL) {
		if (missing(x) || missing(y) || is.null(x) || is.null(y)) {
			stop('please provide both x and y')
		}

		# if (is.null(direction)) {
		# 	warning(
		# 		'missing direction. it is recommended to provide these for
		#subsequent selection of camera trap locations.'
		# 	)
		# }
		#
		# check_direction(direction)

		# direction = NULL,
		# coords = NULL,
		# crs = NULL) {
		# if (!(is.null(coords))) {
		# 	warning('coords ignored since x is an sf object')
		# }

		# set_eval_attr(
		distanceto::distance_to(y, x, measure = measure)
		# 	layer = deparse(substitute(layer)),
		# 	type = 'real',
		# 	direction = direction
		# )[]

	}
