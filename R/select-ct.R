#' Select camera trap locations
#'
#' Select camera trap locations by subsetting (`sub`) and ranking (`rank`) locations in each group (`by`).
#'
#' `n` is the number of locations to select. When groups are defined with `by`, the number of locations will be selected for each group.
#'
#' `sub` is a named list used to subset the input `x`. It should follow the form `list(colname = value)`or `list(colname = c(values, values))`.
#'
#' `by` is a character vector of column names in `x` to group camera trap locations and rank. This should match (at least) the column provided to `strat_sample`, if it was used to generate potential locations.
#'
#'
#' @inheritParams make_grid
#' @param n number of locations to select. if `by` is provided, `select_ct` will select `n` for each group defined in `by`.
#' @param rank character vector of column name(s) in `x` to rank rows.
#' @param sub a named list with the form `list(colname = value)`. See Details.
#' @param by character vector of column name(s) in `x` to form groups. if `by` is provided, ranking and subsetting will occur within in each group.
#'
#' @return
#' @export
#'
#' @examples
select_ct <- function(x, n, rank = NULL, sub = NULL, by = NULL) {
	# NAs detected, removing
	# na.omit

	if (missing(x)) {
		stop('x is required. either a data.table or sf object.')
	}

	if (missing(n)) {
		stop('n is required.')
	}

	if (!is.numeric(n) | n < 1) {
		stop('n must be a positive numeric.')
	}

	if (is.null(rank) & is.null(sub) & is.null(by)) {
		warning('rank, sub and by are all NULL... selecting n rows  arbitrarily')
	}



	directions <- vapply(rank, function(col) parse_directions(x, col), 1L)


	if (inherits(x, 'sf')) {
		t <- 'sf'
		x <- as.data.table(x)
	} else if (inherits(x, 'data.table')) {
		t <- 'dt'
	} else {
		stop('x must be either a data.table or an sf object')
	}

	sel <- function(x) {
		if (is.null(sub)) {
			data.table::setorderv(
				x,
				cols = c(by, names(directions)),
				order = c(rep(1, length(by)), directions)
			)[, .SD[seq(1, n)], by]
		} else {
			data.table::setorderv(
				x[sub, on = names(sub)],
				cols = c(by, names(directions)),
				order = c(rep(1, length(by)), directions)
			)[, .SD[seq(1, n)], by]
		}
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
