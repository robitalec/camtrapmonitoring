#' Select camera trap locations
#'
#' ...
#'
#' `sub` is a named list used to subset the input `x`. It should follow the form `list(colname = value)`or `list(colname = c(values, values))`.
#'
#'
#' @param x
#' @param n
#' @param rank
#' @param sub a named list with the form `list(colname = value)`. See Details.
#' @param by
#'
#' @return
#' @export
#'
#' @examples
select_ct <- function(x, n, rank, sub, by = NULL) {
	# NAs detected, removing
	# na.omit

	# should sub do 'lc > 202'?

	# should rank do: camera grid -> summary stat?

	directions <- vapply(rank, function(col) parse_directions(x, col), 1L)


	if (inherits(x, 'sf')) {
		t <- 'sf'
		x <- as.data.table(x)
	} else if (inherits(x, 'data.table')) {
		t <- 'dt'
	} else {
		stop('x must be either a data.table or an sf object')
	}

	nby <- x[, .GRP, by = by][, .N]


	sel <- function(x) {
		data.table::setorderv(
			x[sub, on = names(sub)],
			cols = c(by, names(directions)),
			order = c(rep(1, length(by)), directions)
		)[, .SD[1:n / nby], by]
	}

	if (t == 'dt') {
		return(sel(x))
	} else if (t == 'sf') {
		return(sf::st_as_sf(sel(x)))
	}
}



parse_directions <- function(x, col) {
	d <- attr(x[[col]], 'wildcam')[['direction']]
	if (d == 'positive') {
		1L
	} else if (d == 'negative') {
		-1L
	} else if (d == 'neutral') {
		stop('cannot rank columns with a neutral direction')
	}
}
