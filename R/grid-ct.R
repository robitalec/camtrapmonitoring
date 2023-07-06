#' Make camera trap grids
#'
#' Set up grids around focal points. For example, sample points in your study area and use `grid_ct` to establish a grid of camera traps around each.
#'
#' @param x data.table or sf points.
#' @param n number of points around each focal point. `n` overrides the `case` argument, do not provide both. See details.
#' @param case "queen", "rook" or "bishop". Ignored if `n` is provided.
#' @param distance distance between adjacent camera traps. Don't worry about the hypotenuse.
#' @param id column in `x` indicating id of focal point. Only used when x is a `data.table`.
#' @param coords columns in `x` indicating names of coordinate columns of focal point. Only used when x is a `data.table`. Expects length = 2 e.g.: c('X', 'Y').
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
#' ## Make grid with case
#' queen <- grid_ct(points, case = 'queen', distance = 100)
#' plot(queen)
#'
#' # Focal individuals
#' plot(queen['focal'])
#'
#' rook <- grid_ct(points, case = 'rook', distance = 100)
#' plot(rook)
#'
#' bishop <- grid_ct(points, case = 'bishop', distance = 100)
#' plot(bishop)
#'
#' ## Make grid with n
#' grid <- grid_ct(points, n = 25, distance = 100)
#' plot(grid)
#'
#' # data.table input
#' library(data.table)
#' DT <- data.table(ID = points$ID, st_coordinates(points))
#' grid <- grid_ct(DT, case = 'queen', distance = 100, id = 'ID', coords = c('X', 'Y'))
grid_ct <- function(x,
										n,
										case,
										distance,
										id = NULL,
										coords = NULL) {
	# NSE
	X <- Y <- NULL


	if ((missing(n) & missing(case)) |
			!missing(n) & !missing(case)) {
		stop('provide one of n and case and not both.')
	}

	if (missing(case)) {
		tms <- floor(n / 8)
		s <- seq(1, tms) * distance
		move <- data.table::CJ(X = c(0, -s, s), Y = c(0, -s, s))
		move <- move[order(abs(X) + abs(Y))][1:n]
	} else if (case == 'queen') {
		move <- data.table::CJ(X = c(0, -distance, distance),
													 Y = c(0, -distance, distance))
		move <- move[order(abs(X), abs(Y))]
	} else if (case == 'bishop') {
		move <- rbind(list(0, 0),
									data.table::CJ(X = c(-distance, distance),
																 Y = c(-distance, distance)))
	} else if (case == 'rook') {
		move <- rbind(list(0, 0),
									data.table::data.table(X = c(0, distance, 0, -distance),
																				 Y = c(distance, 0, -distance, 0)))
	} else {
		stop('case provided must be one of "queen", "rook" or "bishop"')
	}


	if (distance < 0 | !is.numeric(distance)) {
		stop('distance must be a numeric, greater than 0')
	}

	grid_ct_(
		x,
		n = n,
		case = case,
		distance = distance,
		id = id ,
		coords = coords,
		move = move
	)
}


grid_ct_ <- function(x,
										 n,
										 case,
										 distance,
										 id = NULL,
										 coords = NULL,
										 move) {
	UseMethod('grid_ct_')
}


#' @export
#' @import data.table
#' @describeIn grid_ct
grid_ct_.data.table <-
	function(x,
					 n,
					 case,
					 distance,
					 id = NULL,
					 coords = NULL,
					 move) {
		# NSE
		camID <- focal <- . <- NULL

		if (is.null(id) | is.null(coords)) {
			stop('id and coords must be provided with x is a data.table')
		}

		if (!(id %in% colnames(x))) {
			stop('id provided not found in colnames(x)')
		}

		if (!(all(coords %in% colnames(x)))) {
			stop('coords provided not found in colnames(x)')
		}

		out <- x[rep(1:.N, each = nrow(move))]
		set(out, j = coords[[1]],
				value = out[[coords[[1]]]] + as.double(move$X))
		set(out, j = coords[[2]],
				value = out[[coords[[2]]]] + as.double(move$Y))
		out[, camID := .I]

		focals <- out[, .(camID = min(camID)), id]
		out[, focal := ifelse(camID %in% focals$camID, TRUE, FALSE)]
		return(out)
	}



#' @export
#' @describeIn grid_ct
grid_ct_.sf <- function(x,
												n,
												case,
												distance,
												id = NULL,
												coords = NULL,
												move) {
	if (!('geometry' %in% colnames(x))) {
		stop('geometry column not found in x')
	}

	if (!inherits(x[['geometry']], 'sfc_POINT')) {
		stop('class of geometry column must be sfc_POINT')
	}

	r <- x[rep(1:nrow(x),  each = nrow(move)), ]

	out <- sf::st_as_sf(data.frame(r[, colnames(r)[!(grepl('geometry', colnames(r),
																												 fixed = TRUE))]],
																 sf::st_coordinates(r) +
																 	as.matrix(move[rep(1:.N, times = nrow(x))])),
											coords = c('X', 'Y'))

	out$camID <- seq.int(1, nrow(out))

	focals <- by(out, out$ID, function(x)
		min(x$camID))
	out$focal <- ifelse(out$camID %in% focals, TRUE, FALSE)

	if (is.null(sf::st_crs(x))) {
		return(out)
	} else {
		sf::st_crs(out) <- sf::st_crs(x)
		return(out)
	}
}
