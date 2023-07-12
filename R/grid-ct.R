#' Make camera trap grids
#'
#' Set up grids around focal points. For example, sample points in your study
#' area with `sample_ct` then use `grid_ct` to establish a grid of camera traps
#' around each.
#'
#' @inheritParams sample_ct
#' @param case "queen", "rook" or "bishop". Ignored if `n` is provided.
#' @param distance distance between adjacent camera traps. Don't worry about the hypotenuse.
#' @param id default: "id_sample_ct" generated automatically from `sample_ct`
#' @param n number of points around each focal point. `n` overrides the `case` argument, do not provide both - see Details.

#' @return
#'
#' Extended data.table either nine times the length of input DT for 'queen' case or 5 times the length of input DT for 'rook' or 'bishop' case. See examples.
#'
#' The logical 'focal' column indicates which point is the focal or center camera trap.
#'
#' @export
#'
#' @examples
#' data("clearwater_lake_density")
#' pts <- sample_ct(clearwater_lake_density, 1, type = 'random')
#'
#' # Make grid with case, eg. 'queen'
#' queen <- grid_ct(pts, case = 'queen', distance = 100)
#'
#' # Plot
#' plot(queen['focal'])
#'
#' # Make grid with n
#' n_grid <- grid_ct(pts, n = 25, distance = 100)
#' plot(n_grid['id_grid_ct'])
grid_ct <- function(x,
										n,
										case,
										distance) {


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

	sf::st_crs(out) <- sf::st_crs(x)
	return(out)
}
