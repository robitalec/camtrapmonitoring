#' Binary layer
#'
#' Make a binary raster layer from input 'layer'.
#'
#' Find all pixels matching the 'value' provided given the 'fun' and return a binary raster.
#'
#' @inheritParams eval_pt
#' @param value numeric value in 'layer'. see Details.
#' @param fun character indicating which function to use to compare layer to value. One of 'equal', 'gt', 'gte', 'lt', 'lte' or 'in'.
#'
#' @return
#' A binary raster layer with two values: `TRUE` if pixel matches 'value' provided and `FALSE` if pixel does not match 'value' provided.
#'
#' 'value' may only be length 1 if 'fun' is one of: 'equals', 'gt', 'gte', 'lt', 'lte'.
#' 'value' may be greater than length 1 if 'fun' is: 'in'.
#'
#' @export
#'
#' @examples
#' data(lc)
#'
#' bin <- binary_layer(lc, 212, fun = 'equals')
#'
#' image(bin)
binary_layer <- function(layer, value, fun) {
	if (missing(layer)) {
		stop('layer must be provided.')
	}

	if (missing(value)) {
		stop('value must be provided.')
	}

	if (missing(fun)) {
		if (length(value) == 1) {
			message('argument "fun" is missing, defaulting to "equals"')
		} else if (length(value) > 1) {
			message('argument "fun" is missing, defaulting to "in"')
		}
	}

	if (!inherits(layer, 'Raster')) {
		stop('layer must be a raster.')
	}

	if (!inherits(value, 'numeric')) {
		stop('value must be a numeric.')
	}

	if (length(value) == 1) {
		if (fun == 'equals') {
			return(layer == value)
		} else if (fun == 'gt') {
			return(layer > value)
		} else if (fun == 'gte') {
			return(layer >= value)
		} else if (fun == 'lt') {
			return(layer < value)
		} else if (fun == 'lte') {
			return(layer <= value)
		} else {
			stop('fun must be "equals", "gt", "gte", "lt", or "lte" if length of value is 1')
		}
	} else if (length(value) > 1) {
		if (fun == 'in') {
			return(layer %in% value)
		} else {
			stop('fun must be "in" if length of value is > 1')
		}
	}


}
