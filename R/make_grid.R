#' Make camera trap grids
#'
#' Set up grids around focal points. For example, sample points in your study area and use `make_grid` to establish a grid of camera traps around each.
#'
#' @param x data.table or sf points.
#' @param case "queen", "rook" or "bishop".
#' @param distance distance between adjacent camera traps. Don't worry about the hypotenuse.
#' @param id id of focal point.
#' @param coords names of coordinate columns.
#'
#' @return
#'
#' Extended data.table either nine times the length of input DT for 'queen' case or 5 times the length of input DT for 'rook' or 'bishop' case. See examples.
#'
#' The logical 'focal' column indicates which point is the focal or center camera trap.
#'
#' @export
#'
#' @examples
#' # Point data (sf object)
#' library(sf)
#' data(points)
#' plot(points)
#'
#' queen <- make_grid(points, case = 'queen', distance = 100)
#' plot(queen)
#'
#' rook <- make_grid(points, case = 'rook', distance = 100)
#' plot(rook)
#'
#' bishop <- make_grid(points, case = 'bishop', distance = 100)
#' plot(bishop)
#'
#' # Or a data.table
#' library(data.table)
#' DT <- data.table(ID = points$ID, st_coordinates(points))
#' make_grid(DT, case = 'queen', distance = 100, id = 'ID', coords = c('X', 'Y'))
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

	move <- move[order(abs(V1), abs(V2))]

	UseMethod('make_grid', x)
}

#' @export
#' @import data.table
make_grid.data.table <- function(x, case, distance, id, coords) {
	# NSE
	focal <- camX <- camY <- NULL;

	if (is.null(id) | is.null(coords)) {
		stop('id and coords must be provided with x is a data.table')
	}

	if (!(id %in% colnames(x))) {
		stop('id provided not found in colnames(x)')
	}

	if (!(all(coords %in% colnames(x)))) {
		stop('coords provided not found in colnames(x)')
	}

	out <- DT[rep(1:.N, times = nrow(move))]
	out[, c('X', 'Y') := .SD + move,
		 .SDcols = coords, by = id]

	out[1:nrow(DT), focal := TRUE]
	out[is.na(focal), focal := FALSE][]

	return(out)
}


#' @export
make_grid.sf <- function(x, case, distance) {
	if (!('sfc_POINT' %in% class(x$geometry))) {
		stop('class of geometry column must be sfc_POINT')
	}

	r <- x[rep(1:nrow(x),  each = nrow(move)),]

	out <- sf::st_as_sf(
		data.frame(r[, colnames(r)[!(grepl('geometry', colnames(r),
																			 fixed = TRUE))]],
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
