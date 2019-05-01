context("test-scale-roi")

test_that("scale_layer works", {
	expect_error(
		scale_layer(layer = NULL, roi = densitygrid),
		'layer must be provided and expected type is raster.'
	)


	expect_error(
		scale_layer(layer = dem, roi = NULL),
		'roi must be provided.'
	)


	expect_error(
		scale_layer(layer = 'a', roi = densitygrid),
		'layer must be provided and expected type is raster.'
	)

})
