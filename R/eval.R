#' Evaluate camera trap locations by point sampling layers
#'
#' Using camera trap locations generated with `camtrapmonitoring`
#' functions [sample_ct()] and [grid_ct()], sample raster layers to
#' characterize and select camera trap locations, and quantify potential
#' sampling bias.
#'
#'
#' @param features sf features (see [sf::st_sf()])
#' @param target SpatRaster target (see [terra::rast()])
#' @param layer default 1, see [terra::extract()]
#'
#' @return vector of values from target matching locations in features
#' @export
#'
#' @family eval
#'
#' @examples
#' library(terra)
#'
#' data("clearwater_lake_density")
#' clearwater_lake_land_cover <- rast(system.file('extdata',
#'   'clearwater_lake_land_cover.tif', package = 'camtrapmonitoring'))
#'
#' # Sample points
#' pts <- sample_ct(region = clearwater_lake_density, n = 1, type = 'random')
#'
#' # Make grid with queen's case
#' queen <- grid_ct(features = pts, case = 'queen', distance = 100)
#'
#' # Evaluate each point with the land cover layer
#' queen$lc <- eval_pt(features = queen, target = clearwater_lake_land_cover)
#'
#' plot(queen["lc"])
eval_pt <-
	function(features,
					 target,
					 layer = 1) {
		if (missing(target) || is.null(target) || !inherits(target, 'SpatRaster')) {
			stop('target must be provided. expected type is SpatRaster.')
		}
		if (missing(features) || is.null(features)) {
			stop('features must be provided.')
		}

		stopifnot('features is not of class sf' = inherits(features, 'sf'))
		stopifnot('features is not of geometry type POINT' =
								sf::st_geometry_type(features, FALSE) == 'POINT')

		terra::extract(
			x = target,
			y = features,
			layer = layer,
			simple = TRUE,
			ID = FALSE
		)[[layer]]
	}


#' Evaluate camera trap locations by buffered sampling of layers
#'
#' Using buffered camera trap locations generated with `camtrapmonitoring`
#' functions [sample_ct()] and [grid_ct()], sample raster layers to
#' characterize and select camera trap locations, and quantify potential
#' sampling bias.
#'
#'
#' @inheritParams eval_pt
#' @param buffer_size radius of buffer around each point
#' @param buffer_fun function for summarizing buffer region, default mean
#'
#' @return vector of values from target matching buffered locations in features
#' @export
#'
#' @family eval
#'
#' @examples
#' library(terra)
#'
#' data("clearwater_lake_density")
#' clearwater_lake_elevation <- rast(system.file('extdata',
#'   'clearwater_lake_elevation.tif', package = 'camtrapmonitoring'))
#'
#' # Sample points
#' pts <- sample_ct(region = clearwater_lake_density, 1, type = 'random')
#'
#' # Make grid with queen's case
#' queen <- grid_ct(features = pts, case = 'queen', distance = 100)
#'
#' # Evaluate each point with the land cover layer
#' queen$elev <- eval_buffer(
#'   features = queen, target = clearwater_lake_elevation, buffer_size = 150)
#'
#' plot(queen["elev"])
eval_buffer <-
	function(features,
					 target,
					 buffer_size,
					 buffer_fun = mean,
					 layer = 1) {

		if (missing(target) || is.null(target) || !inherits(target, 'SpatRaster')) {
			stop('target must be provided. expected type is SpatRaster.')
		}
		if (missing(features) || is.null(features)) {
			stop('features must be provided.')
		}

		if (any(buffer_size < terra::res(target))) {
			warning("buffer_size is less than the target's resolution")
		}

		# TODO: add crs = crs(layer)

		stopifnot('features is not of class sf' = inherits(features, 'sf'))
		# stopifnot('features is not of geometry type POINT' =
		# 						sf::st_geometry_type(features, FALSE) == 'POINT')


		terra::extract(
			x = target,
			y = sf::st_buffer(features, dist = buffer_size),
			layer = layer,
			fun = buffer_fun,
			ID = FALSE
		)[[layer]]
	}



#' Evaluate distance-to
#'
#' Using camera trap locations generated with `camtrapmonitoring`
#' functions [sample_ct()] and [grid_ct()], evalaute the distance between
#' features and camera trap locations to characterize and select locations,
#' and quantify potential sampling bias.
#'
#' To avoid the large overhead of creating distance to rasters for small/medium
#'  number of sample points, this function uses the vector-based distance
#'  approach from [distanceto::distance_to()]. It determines the
#'  nearest feature to each target then calculates the distance between
#'  each pair.
#'
#' @inheritParams eval_pt
#' @param target sf feature target (see [sf::st_sf()])
#' @param measure measure type see [geodist::geodist()] for details
#'
#' @return vector of distances between target and features
#'
#' @family eval
#' @export
#'
#' @examples
#' data("clearwater_lake_density")
#' data("clearwater_lake_wetlands")
#'
#' # Sample points
#' pts <- sample_ct(region = clearwater_lake_density, 1, type = 'random')
#'
#' # Make grid with queen's case
#' queen <- grid_ct(features = pts, case = 'queen', distance = 100)
#'
#' # Evaluate each point with the land cover layer
#' queen$dist_wetland <- eval_dist(features = queen, target = clearwater_lake_wetlands)
#'
#' # Plot
#' plot(queen["dist_wetland"])
eval_dist <-
	function(features,
					 target,
					 measure = NULL) {
		if (missing(target) || is.null(target)) {
			stop('please provide target')
		}
		if (missing(features) || is.null(features)) {
			stop('please provide features')
		}

		distanceto::distance_to(features, target, measure = measure)
	}
