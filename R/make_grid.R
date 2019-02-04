#' Camera grid
#'
#' Set up a camera grid around a focal point.
#'
#' @param DT data.table.
#' @param id id of focal point.
#' @param case "queen", "rook" or "bishop".
#' @param distance distance in x and y between cameras. don't worry about the hypotenuse.
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
#' grid <- camera_grid(DT, id = 'point', case = 'queen', distance = 250, coords = c('X', 'Y'))
camera_grid <- function(DT, id, case, distance, coords) {
	# NSE
	focal <- camX <- camY <- NULL;

	if (case == 'queen') {
		move <- data.table::CJ(c(-distance, 0, distance),
													 c(-distance, 0, distance))
	} else if (case == 'rook') {
		move <- data.table::CJ(c(-distance, distance),
													 c(-distance, distance))
	} else if (case == 'bishop') {
		move <- data.table::data.table(c(0, distance, 0, -distance),
																	 c(distance, 0, -distance, 0))
	}

	DT <- DT[rep(1:.N, times = nrow(move))]
	DT[, c('camX', 'camY') := .SD + move,
		 .SDcols = coords,
		 by = id]
	DT[camX == get(coords[[1]]) & camY == get(coords[[2]]),
		 focal := TRUE]
}
