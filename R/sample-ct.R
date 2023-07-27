#' Camera trap sampling
#'
#' Sample potential camera trap locations. For stratified sampling, provide a
#' suitable column to stratify region by. Alternatively, [sf::st_sample()]
#' is used directly to sample points across all features.
#'
#' @param region spatial feature object across which points will be sampled
#' @param n number of random points. If `strata` is provided,
#' n represents the number of random points per strata
#' @param type type of sampling, see [sf::st_sample()]
#' @param strata column name in region indicating strata
#'
#' @return `sf` (see [sf::st_sf()]) object with POINT geometry
#' @export
#'
#' @examples
#' # Example grid with density levels (High, Medium, Low)
#' data(clearwater_lake_density)
#'
#' # Stratified random points for each density level
#' pts_random <- sample_ct(
#'   region = clearwater_lake_density, n = 20,
#'   type = 'random', strata = 'density')
#'
#' # Plot density grid and sampled points
#' plot(clearwater_lake_density, reset = FALSE)
#' plot(pts_random, add = TRUE, pch = 1, strata = 1)
#'
#' # Regular sampled points across all features
#' pts_regular <- sample_ct(
#'   region = clearwater_lake_density, n = 20, type = 'regular')
#'
#' # Plot density grid and sampled points
#' plot(clearwater_lake_density, reset = FALSE)
#' plot(pts_regular, add = TRUE, pch = 2, strata = 1)
sample_ct <- function(region, n, type, strata = NULL) {
	stopifnot('region is missing' = !missing(type))
	stopifnot('n is missing' = !missing(type))
	stopifnot('type is missing' = !missing(type))

	stopifnot('type must be one of "regular", "random", or "hexagonal"' =
							type %in% c('regular', 'random', 'hexagonal'))


	if (is.null(strata)) {
		out <- sf::st_as_sf(sf::st_sample(region, n, type = type, exact = TRUE))
	} else {
		stopifnot('strata column not found in region' = strata %in% colnames(region))

		lvls <- unique(region[[strata]])

		stratified <- lapply(lvls, function(x) {
			s <- sf::st_sf(
				geometry = sf::st_sample(region[region[[strata]] == x, ], n, type = type,
																 exact = TRUE))
			s[[strata]] <- x
			return(s)
		})

		out <- do.call(rbind, stratified)
	}

	out$id_sample_ct <- seq.int(nrow(out))
	return(out)
}