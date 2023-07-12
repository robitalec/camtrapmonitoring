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
										case,
										distance,
										id = 'id_sample_ct',
										n) {

	if (distance < 0 | !is.numeric(distance)) {
		stop('distance must be a numeric, greater than 0')
	}

	stopifnot('x is of class sf' = inherits(x, 'sf'))
	stopifnot('x is of geometry type POINT' =
							sf::st_geometry_type(x, FALSE) == 'POINT')
	stopifnot(id %in% colnames(x))

	move <- grid_move(case = case, n = n, distance = distance)

	x_rep <- x[rep(seq.int(nrow(x)), each = nrow(move)), ]
	x_rep_coords <- st_coordinates(x_rep)

	move_rep <- move[rep(seq.int(nrow(move)), nrow(x)), ]

	coords_moved <- x_rep_coords + as.matrix(move_rep)
	coords_moved_sf <- st_as_sf(data.frame(coords_moved), coords = c('X', 'Y'))

	x_moved <- st_set_geometry(x_rep, st_geometry(coords_moved_sf))

	x_moved$id_grid_ct <- seq.int(nrow(x_moved))

	focals <- by(x_moved, x_moved[[id]], function(chunk) min(chunk[['id_grid_ct']]))
	x_moved$focal <- ifelse(x_moved[['id_grid_ct']] %in% focals, TRUE, FALSE)

	st_crs(x_moved) <- st_crs(x)
	return(x_moved)
}



grid_move <- function(case, n, distance) {
	if ((missing(n) & missing(case)) |
			!missing(n) & !missing(case)) {
		stop('provide one of n and case and not both.')
	}

	if (missing(case)) {
		tms <- ceiling(n / 8)
		s <- seq.int(tms) * distance
		gd <- expand.grid(X = c(0, -s, s), Y = c(0, -s, s))
		abs_dist <- abs(sqrt(rowSums(gd ^ 2)))
		gd[order(abs_dist),][seq.int(n),]
	} else if (case == 'queen') {
		expand.grid(X = c(0, -distance, distance),
								Y = c(0, -distance, distance))
	} else if (case == 'bishop') {
		rbind(
			list(0, 0),
			expand.grid(X = c(-distance, distance),
									Y = c(-distance, distance))
		)
	} else if (case == 'rook') {
		rbind(
			list(0, 0),
			data.frame(X = c(0, distance, 0, -distance),
								 Y = c(distance, 0, -distance, 0))
		)
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
