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



	vapply(rank, parse_attr_dir, 1L)

	#
	# setorder(
	# 	x[sub, on = names(sub)],
	# 	rank,
	#
	# )
	# [order(rank)]#[, .SD[1:n], by]
}



parse_attr_dir <- function(x, col) {
	d <- attr(x[[col]], 'wildcam')[['direction']]
	if (d == '1') {
		1L
	} else if (d == 'negative') {
		-1L
	} else if (d == 'neutral') {
		stop('cannot rank with a neutral direction')
	}
}
