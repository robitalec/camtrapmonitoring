context('test-binary')

# Packages
library(terra)
library(sf)

# Data
clearwater_lc_path <- system.file('extdata', 'clearwater_lake_land_cover.tif', package = 'camtrapmonitoring')
lc <- rast(clearwater_lc_path)

# Binary layer
bin <- binary_layer(lc, fun = 'equals', value = 18)


test_that('arguments are checked', {

	expect_true(inherits(bin, 'SpatRaster'))

	expect_error(binary_layer(), 'target must be provided.')

	expect_error(binary_layer(lc), 'value must be provided.')

	expect_error(binary_layer('potato', fun = 'equals', 212),
							 'target must be a SpatRaster.')

	expect_error(binary_layer(lc, fun = 'equals', value = 'potato'),
							 'value must be a numeric.')

	expect_error(binary_layer(lc, fun = 'gte', value = c(212, 210)),
							 'fun must be "in" if length of value is > 1')

	expect_error(binary_layer(lc, fun = 'in', value = 212),
							 'fun must be "equals", "gt", "gte", "lt", or "lte" if length of value is 1')

})

test_that('outputs match inputs', {
	expect_equal(crs(lc), crs(bin))

	expect_equal(res(lc), res(bin))
})


test_that('handles NaN', {
	lc[seq.int(5e5)] <- NaN
	bin <- binary_layer(lc, fun = 'equals', value = 18)

	expect_true(NaN %in% terra::unique(lc, na.rm = FALSE)[[1]])
	expect_true(NaN %in% terra::unique(bin, na.rm = FALSE)[[1]])
})
