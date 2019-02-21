dist_to_raster <- function(points, r, val) {
	rpt <- sf::st_as_sf(as(raster::mask(lc, lc, maskvalue = val),
												 "SpatialPoints"))

	sf::st_distance(points, rpt[sf::st_nearest_feature(points, rpt),],
									by_element = TRUE)
}

system.time(
	dist_to_raster(strat_sample(densitygrid, n = 1e6, 'density', returnDT = FALSE),
								 lc,
								 20)
)
