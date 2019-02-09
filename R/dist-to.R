#' Distance-to
#'
#' Calculate the distance from features in x to the nearest feature in y.
#'
#' To avoid the large overhead of creating distance to rasters for small/medium number of sample points, this vector-based distance to determines the nearest feature (y) to each x then calculates the distance between each pair.
#'
#' @param x object of class sfg, sfc or sf
#' @param y object of class sfg, sfc or sf
#'
#' @return Vector of distances between x and the nearest feature in y.
#' @export
#'
#' @examples
#'
#'
#'
dist_to <- function(x, y) {
	if (is.null(x) | is.null(y)) {
		stop('please provide both x and y')
	}

	sf::st_distance(x, y[sf::st_nearest_feature(x, y), ],
									by_element = TRUE)
}
