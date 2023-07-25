context('test-binary')

# Packages
library(terra)
library(sf)

# Data
clearwater_lc_path <- system.file('extdata', 'clearwater_lake_land_cover.tif', package = 'camtrapmonitoring')
lc <- rast(clearwater_lc_path)

# Binary layer
bin <- binary_layer(lc, 18, fun = 'equals')


test_that('arguments are checked', {

	expect_true(inherits(bin, 'SpatRaster'))

	expect_error(binary_layer(), 'x must be provided.')

	expect_error(binary_layer(lc), 'value must be provided.')

	expect_error(binary_layer('potato', 212, fun = 'equals'),
							 'x must be a SpatRaster.')

	expect_error(binary_layer(lc, 'potato', fun = 'equals'),
							 'value must be a numeric.')

	expect_error(binary_layer(lc, c(212, 210), fun = 'gte'),
							 'fun must be "in" if length of value is > 1')

	expect_error(binary_layer(lc, 212, fun = 'in'),
							 'fun must be "equals", "gt", "gte", "lt", or "lte" if length of value is 1')

})

test_that('outputs match inputs', {
	expect_equal(crs(lc), crs(bin))

	expect_equal(res(lc), res(bin))
})


	buf <- st_buffer(points[1,], 1e2)
	mlc <- mask(crop(lc, buf), buf, inverse = TRUE)

	expect_true(NA %in% raster::unique(mlc, na.last = TRUE))

	expect_true(NA %in% raster::unique(binary_layer(mlc, 212, fun = 'equals'), na.last = TRUE))
})
