#' Stratified polygon sampling
#'
#' For each unique value in 'col', sample 'n' points and optionally return as a \code{data.table}.
#'
#' @param x polygon object of class sf
#' @param n number of random points
#' @param col column in x indicating strata
#' @param returnDT return a data.table (TRUE) or sf (FALSE) object
#'
#' @return Either a \code{sf} object or a \code{data.table} with a \code{sfc} (simple feature column).
#' @export
#'
#' @examples
#'
#' data(densitygrid)
#'
#' pts <- strat_sample(densitygrid, 5, col = 'density', returnDT = FALSE)
#'
#' plot(densitygrid, reset = FALSE)
#' plot(pts$geometry, add = TRUE)
strat_sample <- function(x, n, col, returnDT = TRUE) {
	lvls <- unique(x[[col]])
	DT <- lapply(lvls, function(l) {
		sf::st_sf(col = l,
							geometry = sf::st_sample(x[x[[col]] == l, ], n, type = 'random'))
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
