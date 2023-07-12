#' Evaluate camera trap locations by point sampling layers
#'
#' Using the point locations generated manually or with `wildcam` functions [sample_ct()] and [grid_ct()], sample raster layers to characterize and select camera trap locations, and quantify potential sampling bias.
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
#' @param x SpatRaster layer (terra package)
#' @param y spatial feature object
#' @param layer default 1, see terra::extract
#' @param type one of 'categorical', 'binary', 'ordinal', or 'real'. See Details.
#' @param direction one of 'positive', 'neutral', 'negative'. See Details.
#'
#' @return
#' @export
#'
#' @family eval
#'
#' @examples
#' data("clearwater_lake_density")
#' clearwater_lake_land_cover <- rast(system.file('extdata', 'clearwater_lake_land_cover.tif', package = 'wildcam'))
#'
#' # Sample points
#' pts <- sample_ct(clearwater_lake_density, 1, type = 'random')
#'
#' # Make grid with queen's case
#' queen <- grid_ct(pts, case = 'queen', distance = 100)
#'
#' # Evaluate each point with the land cover layer
#' queen$lc <- eval_pt(x = points, layer = clearwater_lake_land_cover, type = 'categorical', direction = 'neutral')
#'
#' plot(queen["lc"])
eval_pt <-
	function(x,
					 y,
					 layer = 1,
					 type = NULL,
					 direction = NULL,
					 coords = NULL) {
		if (missing(x) || is.null(x) || !inherits(x, 'SpatRaster')) {
			stop('x must be provided. expected type is SpatRaster.')
		}
		if (missing(y) || is.null(y)) {
			stop('y must be provided. either data.table or sf point object.')
		}

		# if (is.null(type) || is.null(direction)) {
		# 	warning(
		# 		'missing type and/or direction. it is recommended to provide these for subsequent selection of camera trap locations.'
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
#' Using the buffered points locations generated manually or with `wildcam` functions [sample_ct()] and [grid_ct()], sample raster layers to characterize and select camera trap locations, and quantify potential sampling bias.
#'
#' Type is used to define the data type of the sampled raster layer:
#'
#' * 'categorical' - one of a limited and usually fixed number of possible values (e.g.: landcover. 1, 2, 3 indicating 'wetland', 'forest', 'agriculture').
#' * 'binary' - also boolean. TRUE/FALSE, presence/absence. (e.g.: raster of wetlands).
#' * 'ordinal' - ranked or ordered data. (e.g.: animal density high/medium/low).
#' * 'real' - real number variable either interval or ratio scale. (e.g.: digital elevation model).
#'
#' The function used to summarize the buffer regions around each point in `x` depends on the data `type` defined. 'categorical' data should likely be transformed into individual binary layers with [binary_layer()] so `mean` can be used to determine proportions in a buffer region.
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
#' library(terra)
#' data("clearwater_lake_density")
#' clearwater_lake_elevation <- rast(system.file('extdata', 'clearwater_lake_elevation.tif', package = 'wildcam'))
#'
#' # Sample points
#' pts <- sample_ct(clearwater_lake_density, 1, type = 'random')
#'
#' # Make grid with queen's case
#' queen <- grid_ct(pts, case = 'queen', distance = 100)
#'
#' # Evaluate each point with the land cover layer
#' queen$elev <- eval_buffer(x = clearwater_lake_elevation, y = queen, buffer_size = 150)
#'
#' plot(queen["elev"])
eval_buffer <-
	function(x,
					 y,
					 buffer_size,
					 buffer_fun = mean,
					 layer = 1,
					 type,
					 direction,
					 coords = NULL) {

		if (missing(x) || is.null(x) || !inherits(x, 'SpatRaster')) {
			stop('x must be provided. expected type is SpatRaster.')
		}
		if (missing(y) || is.null(y)) {
			stop('y must be provided. either data.table or sf point object.')
		}

		# if (is.null(type) || is.null(direction)) {
		# 	warning('missing type and/or direction. it is recommended to provide these for subsequent selection of camera trap locations.')
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
		# 		warning('type provided is either categorical or ordinal, cannot summarize in buffer, returning frequency table')
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
#' Evaluates locations in x by measuring the distance to the nearest feature in layer.
#'
#' To avoid the large overhead of creating distance to rasters for small/medium number of sample points, this vector-based distance to determines the nearest feature (layer) to each x then calculates the distance between each pair.
#'
#' @inheritParams eval_pt
#' @param layer object of class sfg, sfc or sf.
#' @param crs coordinate reference system of the coordinates in x, if x is a data.table. Either an integer with the EPSG code, or character with proj4string (see the 'crs' argument in \link[sf]{st_sf}).
#'
#' @param measure measure type see geodist::geodist for details
#'
#' @return Vector of distances between x and the nearest feature in layer.
#'
#' Note attributes are returned like by `eval_pt` and `eval_buffer`. The `type` attribute for distance to a feature (layer) is "real" and the `direction` is left for the user to provide.
#'
#' @family eval
#' @export
#'
#' @examples
#' # sf objects
#' data(water)
#' data(points)
#'
#' points$distWater <- eval_dist(points, water, direction = 'negative')
#'
#' # data.table objects
#' library(data.table)
#'
#' data(DT)
#' alloc.col(DT)
#'
#' DT[, distWater := eval_dist(.SD, water, coords = c('X', 'Y'), direction = 'positive', crs = sf::st_crs(water))]
eval_dist <-
	function(x,
					 layer,
					 direction = NULL,
					 coords = NULL,
					 crs = NULL) {
		if (missing(x) || missing(layer) || is.null(x) || is.null(layer)) {
			stop('please provide both x and layer')
		}

		if (is.null(direction)) {
			warning(
				'missing direction. it is recommended to provide these for subsequent selection of camera trap locations.'
			)
		}

		check_direction(direction)

		# direction = NULL,
		# coords = NULL,
		# crs = NULL) {
		if (!(is.null(coords))) {
			warning('coords ignored since x is an sf object')
		}

		set_eval_attr(
			sf::st_distance(x, layer[sf::st_nearest_feature(x, layer),],
											by_element = TRUE),
			layer = deparse(substitute(layer)),
			type = 'real',
			direction = direction
		)[]

	}