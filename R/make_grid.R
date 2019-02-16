#' Make camera trap grids
#'
#' Set up grids around focal points. For example, sample points in your study area and establish a grid of camera traps around each.
#'
#' @param DT data.table or sf points.
#' @param case "queen", "rook" or "bishop".
#' @param distance distance in x and y between cameras. don't worry about the hypotenuse.
#' @param id id of focal point.
#' @param coords names of coordinate columns.
#'
#' @return
#'
#' Extended data.table either nine times the length of input DT for 'queen' case or 5 times the length of input DT for 'rook' or 'bishop' case.
#'
#' The logical 'focal' column indicates which point is the focal.
#'
#' @export
#'
#' @examples
#' # Load point data (sf object)
#' data(points)
#'
#' queen <- make_grid(points, case = 'queen', distance = 250)
#'
#' rook <- make_grid(points, case = 'rook', distance = 250)
#'
#' bishop <- make_grid(points, case = 'bishop', distance = 250)
#'
#' plot(points$X)
#' plot(queen)
#' plot(rook)
#' plot(bishop)
#'
#' # Or with a data.table
#' setDT(points)
#'
#' make_grid(points, id = 'point', case = 'queen', distance = 250, coords = c('X', 'Y'))
#'
make_grid <- function(x, case, distance, ...) {
	if (case == 'queen') {
		move <- data.table::CJ(c(-distance, 0, distance),
													 c(-distance, 0, distance))
	} else if (case == 'rook') {
		move <- data.table::CJ(c(-distance, distance),
													 c(-distance, distance))
	} else if (case == 'bishop') {
		move <- data.table::data.table(c(0, distance, 0, -distance),
																	 c(distance, 0, -distance, 0))
	} else {
		stop('must provide case one of "queen", "rook" or "bishop"')
	}

	UseMethod('make_grid', x)
}

#' @export
#' @import data.table
make_grid.data.table <- function(x, case, distance, id, coords) {
	# NSE
	focal <- camX <- camY <- NULL;

	if (!(id %in% colnames(x))) {
		stop('id provided not found in colnames(x)')
	}

	if (!(all(coords %in% colnames(x)))) {
		stop('coords provided not found in colnames(x)')
	}

	DT <- DT[rep(1:.N, times = nrow(move))]
	DT[, c('camX', 'camY') := .SD + move,
		 .SDcols = coords,
		 by = id]

	DT[camX == get(coords[[1]]) & camY == get(coords[[2]]),
		 focal := TRUE]
	DT[is.na(focal), focal := FALSE]

	return(DT)
}


#' @export
make_grid.sf <- function(x, case, distance) {
	if (!('sfc_POINT' %in% class(x$geometry))) {
		stop('class of geometry column must be sfc_POINT')
	}

	r <- x[rep(1:nrow(x),  each = nrow(move)),]

	out <- sf::st_as_sf(
		data.frame(r[, colnames(r)[!(grepl('geometry', colnames(r)))]],
							 st_coordinates(r) +
							 	as.matrix(move[rep(1:.N, times = nrow(x))])),
		coords = c('X', 'Y')
	)

	if (is.null(sf::st_crs(x))) {
		return(out)
	} else {
		sf::st_crs(out) <- sf::st_crs(x)
		return(out)
	}

}
