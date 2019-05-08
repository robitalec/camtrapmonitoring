#' Select camera trap locations
#'
#' Select camera trap locations by subsetting (`sub`) and ranking (`rank`) locations in each group (`by`).
#'
#' `n` is the number of locations to select. When groups are defined with `by`, the number of locations will be selected for each group.
#'
#' `sub` is a named list used to subset the input `x`. It should follow the form `list(colname = value)`or `list(colname = c(values, values))`.
#'
#' `by` is a character vector of column names in `x` to group camera trap locations and rank. This should match (at least) the column provided to `sample_ct`, if it was used to generate potential locations.
#'
#' Note: NAs are omitted from input x before select camera trap locations.
#'
#'
#' @inheritParams grid_ct
#' @param n number of locations to select. if `by` is provided, `select_ct` will select `n` for each group defined in `by`.
#' @param rank character vector of column name(s) in `x` to rank rows.
#' @param sub a named list with the form `list(colname = value)`. See Details.
#' @param by character vector of column name(s) in `x` to form groups. if `by` is provided, ranking and subsetting will occur within in each group.
#'
#' @return
#' @export
#'
#' @examples
#' # Packages
#' library(data.table)
#'
#' # Data
#' data(densitygrid)
#' data(lc)
#' data(dem)
#' data(wetland)
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
#' # Select n locations
#' n <- 1
#'
#' sel <- select_ct(pts, n, rank = c('wetland'), sub = list(lc = 212), by = 'density')
#'
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
	if (any(!(c(rank, names(sub), by) %in% colnames(x)))) {
		stop('column names in rank, sub and/or not found in x')
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
				stats::na.omit(x),
				cols = c(by, names(directions)),
				order = c(rep(1, length(by)), directions)
			)[, .SD[seq(1, n)], by]
		} else {
			data.table::setorderv(
				stats::na.omit(x)[sub, on = names(sub)],
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
