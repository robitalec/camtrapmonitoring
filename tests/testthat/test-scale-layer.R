context("test-scale-layer")


# Load packages
library(terra)

# Load data
data("clearwater_lake_hydro")
clearwater_elev_path <- system.file("extdata", "clearwater_lake_elevation.tif", package = "camtrapmonitoring")
clearwater_lake_elevation <- rast(clearwater_elev_path)

region <- clearwater_lake_hydro[4,]



test_that("scale_layer checks args", {
	expect_error(
		scale_layer(target = NULL, region = region),
		'target must be provided'
	)


	expect_error(
		scale_layer(target = clearwater_lake_elevation, region = NULL),
		'region must be provided'
	)


	expect_error(
		scale_layer(target = 'a', region = region),
		'target must be a SpatRaster'
	)

})
