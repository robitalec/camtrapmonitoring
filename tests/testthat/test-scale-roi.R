context("test-scale-roi")


# Load packages
library(terra)

# Load data
clearwater_elev_path <- system.file("extdata", "clearwater_lake_elevation.tif", package = "camtrapmonitoring")
clearwater_lake_elevation <- rast(clearwater_elev_path)

# Region of interest: Clearwater lake area
roi <- ext(clearwater_lake_elevation)



test_that("scale_layer works", {
	expect_error(
		scale_layer(x = NULL, roi = densitygrid),
		'x must be provided and expected type is SpatRaster.'
	)


	expect_error(
		scale_layer(x = clearwater_lake_elevation, roi = NULL),
		'roi must be provided.'
	)


	expect_error(
		scale_layer(x = 'a', roi = densitygrid),
		'x must be provided and expected type is SpatRaster.'
	)

})
