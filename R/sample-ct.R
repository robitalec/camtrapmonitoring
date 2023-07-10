#' Camera trap sampling
#'
#' Sample potential camera trap locations in each region defined by unique values in col in x.
#'
#' Random or regular sampling. Polygons cannot be assigned to multiple values.
#'
#' If you'd like to sample a polygon, but not stratified by any `col`, simply use \link[sf]{st_sample}.
#'
#' @param x polygon object of class `sf`
#' @param n number of random points
#' @param type of sampling. either 'random' or 'regular'.
#' @param col column in x indicating strata
#'
#' @return `sf` object
#' @export
#'
#' @examples
#' # Example polygons with density levels High, Medium, Low
#' data(clearwater_lake_density)
#'
#' # Randomly sample 5 points for each density level
#' pts <- sample_ct(x = clearwater_lake_density, n = 5, type = 'random',
#' col = 'density')
#'
#' plot(clearwater_lake_density, reset = FALSE)
#' plot(pts$geometry, add = TRUE)
#'
#' # Sample 5 regular points for each set of polygons in each strata
#' pts <- sample_ct(x = clearwater_lake_density, n = 20, type = 'regular',
#' col = 'density', returnDT = FALSE)
#'
#' plot(clearwater_lake_density, reset = FALSE)
#' plot(pts$geometry, add = TRUE)
sample_ct <- function(x, n, type, col = NULL) {


	stopifnot('type is missing' = !missing(type))

	# stopifnot('type must be either "regular" or "random"' =
	# 						type %in% c('regular', 'random'))


	# if (is.null(lvls)) {
	# 	stop('no strata found')
	# }


	DT <- lapply(lvls, function(l) {
		s <- sf::st_sf(
			geometry = sf::st_sample(x[x[[col]] == l, ], n, type = type,
															 exact = TRUE))
		s[[col]] <- l
		return(s)
	})

	if (returnDT) {
		out <-
			data.table::rbindlist(DT)[,
																c('X', 'Y') := data.table::as.data.table(sf::st_coordinates(geometry))]
		data.table::set(out, j = 'geometry', value = NULL)
		data.table::set(out, j = 'ID', value = 1:nrow(out))
		return(out)
	} else {
		out <- do.call(rbind, DT)
		out$ID <- 1:nrow(out)
		return(out)
	}
}