#' Select camera trap locations
#'
#' @param x
#' @param n
#' @param rank
#' @param sub
#' @param by
#'
#' @return
#' @export
#'
#' @examples
select_ct <- function(x, n, rank, sub, by) {
	# sub is a named list e.g.: list(lc = 212)
	# rank is a vector of column names
	# by is a vector of column names


	# NAs detected, removing
	# na.omit



	directions <- vapply(rank, function(col) parse_directions(x, col), 1L)


	### x[sub][order(rank)][, .SD[1:n], by] ###
	data.table::setorderv(
		x[sub, on = names(sub)],
		cols = c(by, names(directions)),
		order = c(rep(1, length(by)), directions)
	)[, .SD[1:n], by]
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
