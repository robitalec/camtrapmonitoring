make_binary <- function(r, val) {
	is.na(raster::mask(r, r, maskvalue = val))
}
