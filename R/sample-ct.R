#' Camera trap sampling
#'
#' Sample potential camera trap locations. For stratified sampling, provide a
#' suitable column to stratify by. Alternatively, \link[sf]{st_sample} is used
#' directly to sample points across all features.
#'
#' @param x spatial feature object
#' @param n number of random points, if a column name is provided to `col` n represents the number of random points per strata
#' @param type type of sampling, see \link[sf]{st_sample}
#' @param col column name in x indicating strata
#'
#' @return `sf` object with POINT geometry
#' @export
#'
#' @examples
#' # Example grid with density levels (High, Medium, Low)
#' data(clearwater_lake_density)
#'
#' # Stratified random points for each density level
#' pts_random <- sample_ct(x = clearwater_lake_density, n = 20, type = 'random', col = 'density')
#'
#' # Plot density grid and sampled points
#' plot(clearwater_lake_density, reset = FALSE)
#' plot(pts_random, add = TRUE, pch = 1)
#'
#' # Regular sampled points across all features
#' pts_regular <- sample_ct(x = clearwater_lake_density, n = 20, type = 'regular')
#'
#' # Plot density grid and sampled points
#' plot(clearwater_lake_density, reset = FALSE)
#' plot(pts_regular, add = TRUE, pch = 2)
sample_ct <- function(x, n, type, col = NULL) {
	stopifnot('x is missing' = !missing(type))
	stopifnot('n is missing' = !missing(type))
	stopifnot('type is missing' = !missing(type))

	stopifnot('type must be one of "regular", "random", or "hexagonal"' =
							type %in% c('regular', 'random', 'hexagonal'))


	if (is.null(col)) {
		out <- sf::st_as_sf(sf::st_sample(x, n, type = type, exact = TRUE))
	} else {
		stopifnot('col not found in x' = col %in% colnames(x))

		strata <- unique(x[[col]])

		stratified <- lapply(strata, function(y) {
			s <- sf::st_sf(
				geometry = sf::st_sample(x[x[[col]] == y, ], n, type = type,
																 exact = TRUE))
			s[[col]] <- y
			return(s)
		})

		out <- do.call(rbind, stratified)
	}

	out$id_sample_ct <- seq.int(nrow(out))
	return(out)
}