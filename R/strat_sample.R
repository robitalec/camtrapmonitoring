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
strat_sample <- function(x, n, col, returnDT = TRUE) {
	lvls <- unique(x[[col]])
	DT <- lapply(lvls, function(l) {
		sf::st_sf(col = l,
							geometry = sf::st_sample(x[x[[col]] == l, ], n, type = 'random'))
	})

	if (returnDT) {
		return(rbindlist(DT)[, ID := 1:.N])
	} else {
		out <- do.call(rbind, DT)
		out$ID <- 1:nrow(out)
		return(out)
	}
}
