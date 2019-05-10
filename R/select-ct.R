#' Select camera trap locations
#'
#' Select camera trap locations by subsetting (`sub`) and ranking (`rank`) locations in each group (`by`).
#'
#' `n` is the number of locations to select. When groups are defined with `by`, the number of locations will be selected for each group.
#'
#' `sub` is an expression used to subset the input `x`. It should not be quoted and follow the form described by \link[data.table]{data.table}'s argument `i`. **Note:** if the column provided has a unit (e.g.: those returned by `eval_dist`), the expression should use those units as well. For example, `sub = expression(distwater < as_units(50, 'm')`.
#'
#' `by` is a character vector of column names in `x` to group camera trap locations and rank. This should match (at least) the column provided to `sample_ct`, if it was used to generate potential locations.
#'
#' Note: NAs are omitted from input x before select camera trap locations.
#'
#'
#' @inheritParams grid_ct
#' @param n number of locations to select. if `by` is provided, `select_ct` will select `n` for each group defined in `by`.
#' @param rank character vector of column name(s) in `x` to rank rows.
#' @param sub an expression. See Details.
#' @param by character vector of column name(s) in `x` to form groups. if `by` is provided, ranking and subsetting will occur within in each group.
#'
#' @return
#' @export
#'
#' @examples
#' # Packages
#' library(data.table)
#' library(units)
#' library(sf)
#'
#' # Data
#' data(densitygrid)
#' data(lc)
#' data(dem)
#' data(wetland)
#' data(water)
#'
#' # CRS
#' utm <- st_crs(water)
#'
#' # Stratified random sampling
#' pts <- sample_ct(densitygrid, n = 5, type = 'random', col = 'density', returnDT = TRUE)
#'
#' # Evaluate layers
#' pts[, lc := eval_pt(.SD, lc, type = 'categorical', direction = 'neutral', coords = c('X', 'Y'))]
#'
#' pts[, dem := eval_buffer(.SD, dem, 100, type = 'real', direction = 'positive', coords = c('X', 'Y'))]
#'
#' pts[, wetland := eval_buffer(.SD, wetland, 100, 'binary', 'negative', coords = c('X', 'Y'))]
#'
#' pts[, distwater := eval_dist(.SD, water, direction = 'positive', coords = c('X', 'Y'), crs = utm)]
#'
#' # Select n locations
#' n <- 1
#'
#' sel <- select_ct(pts, n, rank = c('wetland'), sub = expression(distwater > as_units(50, 'm')), by = c('lc', 'density'))
#'
select_ct <- function(x, n, rank = NULL, sub = NULL, by = NULL) {
	if (missing(x) || is.null(x)) {
		stop('x is required. either a data.table or sf object.')
	}

	if (missing(n) || is.null(x)) {
		stop('n is required.')
	}

	if (!is.numeric(n) || n < 1) {
		stop('n must be a positive numeric.')
	}

	if (is.null(rank) && is.null(sub) && is.null(by)) {
		warning('rank, sub and by are all NULL... selecting first n rows arbitrarily')
	}

	if (any(!(vapply(c(rank, by), is.character, FUN.VALUE = TRUE)))) {
		stop('by must be a character or character vector.')
	}

	if (any(!(c(rank, by) %in% colnames(x)))) {
		stop('column names in rank and/or by not found in x.')
	}

	if (!is.null(sub)) {
		if (!is.expression(sub)) {
			stop('sub must be an expression.')
		}
		checkSub <- deparse(sub)

		if (grepl('=', checkSub) && !(grepl('==', checkSub))) {
			stop('check expression provided to sub, found "=", instead of "=="')
		}
	}


	directions <- vapply(rank, function(col) parse_directions(x, col), 1L)

	if (inherits(x, 'sf')) {
		t <- 'sf'
		x <- data.table::as.data.table(x)
	} else if (inherits(x, 'data.table')) {
		t <- 'dt'
	} else {
		stop('x must be either a data.table or an sf object')
	}

	sel <- function(x) {
		if (is.null(sub)) {
			data.table::setorderv(
				stats::na.omit(x),
				cols = c(by, names(directions)),
				order = c(rep(1, length(by)), directions)
			)[, .SD[seq(1, n)], by = by]
		} else {
			data.table::setorderv(
				stats::na.omit(x)[eval(sub)],
				cols = c(by, names(directions)),
				order = c(rep(1, length(by)), directions)
			)[, .SD[seq(1, n)], by = by]
		}
	}

	if (t == 'dt') {
		return(sel(x))
	} else if (t == 'sf') {
		return(sf::st_as_sf(sel(x)))
	}
}


###
parse_directions <- function(x, col) {
	d <- attr(x[[col]], 'wildcam')[['direction']]

	if (is.null(d)) {
		stop('columns in rank do not have direction attribute, did you use eval_*?')
	} else if (d == 'positive') {
		1L
	} else if (d == 'negative') {
		-1L
	} else if (d == 'neutral') {
		stop('cannot rank columns with a neutral direction')
	}
}
