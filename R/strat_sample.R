#' Stratified polygon sampling
#'
#' For each mutually exclusive strata, sample random points.
#'
#' Polygons cannot be assigned to multiple strata. Strata are defined by values in 'col'. Optionally return a `data.table` if 'returnDT' is TRUE or an `sf` object if FALSE.
#'
#' @param x polygon object of class `sf`
#' @param n number of random points
#' @param col column in x indicating strata
#' @param returnDT return a `data.table` (TRUE) or `sf` (FALSE) object
#'
#' @return Either a `sf` object or a `data.table` with a \code{sfc} (simple feature column).
#' @export
#'
#' @examples
#' # Example polygons with density levels 1, 2 and 3.
#' data(densitygrid)
#'
#' # Randomly sample 5 points for each set of polygons in each strata.
#' pts <- strat_sample(x = densitygrid, n = 5, col = 'density', returnDT = FALSE)
#'
#' plot(densitygrid, reset = FALSE)
#' plot(pts$geometry, add = TRUE)
strat_sample <- function(x, n, col, returnDT = TRUE) {
	if (!(col %in% colnames(x))) {
		stop('strata column not found in x')
	}

	lvls <- unique(x[[col]])

	if (is.null(lvls)) {
		stop('no strata found')
	}

	DT <- lapply(lvls, function(l) {
		s <- sf::st_sf(geometry = sf::st_sample(x[x[[col]] == l, ], n, type = 'random'))
		s[[col]] <- l
		return(s)
	})

	if (returnDT) {
		out <- data.table::rbindlist(DT)
		data.table::set(out, j = 'ID', value = 1:nrow(out))
		return(out)
	} else {
		out <- do.call(rbind, DT)
		out$ID <- 1:nrow(out)
		return(out)
	}
}
