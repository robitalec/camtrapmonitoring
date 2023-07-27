#' Make camera trap grids
#'
#' Set up grids around focal points. For example, sample points in your study
#' area with `sample_ct` then use `grid_ct` to establish a grid of camera traps
#' around each.
#'
#' @inheritParams sample_ct
#' @param case "queen", "rook" or "bishop". Ignored if `n` is provided.
#' @param distance distance between adjacent camera traps. Don't worry about
#' the hypotenuse.
#' @param id default: "id_sample_ct" generated automatically from `sample_ct`
#' @param n number of points around each focal point. `n` overrides the `case`
#'  argument, do not provide both - see Details.

#' @return
#'
#' Extended sf object either nine times the length of input features for
#'  'queen' case or 5 times the length of input DT for 'rook' or 'bishop' case.
#'  Otherwise n * number the length of input x. See examples.
#'
#' The logical 'focal' column indicates which point is the focal camera trap for
#' each grid.
#'
#' @export
#'
#' @examples
#' data("clearwater_lake_density")
#' pts <- sample_ct(clearwater_lake_density, 1, type = 'random')
#'
#' # Make grid with case, eg. 'queen'
#' queen <- grid_ct(features = pts, distance = 100, case = 'queen')
#'
#' # Plot
#' plot(queen['focal'])
#'
#' # Make grid with n
#' n_grid <- grid_ct(features = pts, distance = 100, n = 25)
#' plot(n_grid['id_grid_ct'])
grid_ct <- function(features,
										distance,
										case,
										n,
										id = 'id_sample_ct') {

	if (distance < 0 | !is.numeric(distance)) {
		stop('distance must be a numeric, greater than 0')
	}

	stopifnot('features are not class sf' = inherits(features, 'sf'))
	stopifnot('features are not geometry type POINT' =
							sf::st_geometry_type(features, FALSE) == 'POINT')
	stopifnot(id %in% colnames(features))

	move <- grid_design(distance = distance, case = case, n = n)

	x_rep <- features[rep(seq.int(nrow(features)), each = nrow(move)), ]
	x_rep_coords <- sf::st_coordinates(x_rep)

	move_rep <- move[rep(seq.int(nrow(move)), nrow(features)), ]

	coords_moved <- x_rep_coords + as.matrix(move_rep)
	coords_moved_sf <- sf::st_as_sf(data.frame(coords_moved),
																	coords = c('X', 'Y'))

	x_moved <- sf::st_set_geometry(x_rep, sf::st_geometry(coords_moved_sf))

	x_moved$id_grid_ct <- seq.int(nrow(x_moved))

	focals <- by(x_moved, x_moved[[id]], function(chunk) {
		min(chunk[['id_grid_ct']])
	})
	x_moved$focal <- ifelse(x_moved[['id_grid_ct']] %in% focals, TRUE, FALSE)

	sf::st_crs(x_moved) <- sf::st_crs(features)
	return(x_moved)
}



#' Grid design
#'
#' Helper function used internally by `grid_ct` to establish grids around
#' focal locations. Provided to the user to explore grid design options before
#' applying to their data.
#'
#' @inheritParams grid_ct
#'
#' @return
#' @export
#'
#' @examples
#' plot(grid_design(distance = 100, case = 'queen'))
#' plot(grid_design(distance = 100, n = 13))
grid_design <- function(distance, case, n) {
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
}