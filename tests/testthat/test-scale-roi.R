context("test-scale-roi")

test_that("scale_roi works", {
	expect_error(
		scale_roi(layer = NULL, roi = densitygrid),
		'layer must be provided and expected type is raster.'
	)


	expect_error(
		scale_roi(layer = dem, roi = NULL),
		'roi must be provided.'
	)


	expect_error(
		scale_roi(layer = 'a', roi = densitygrid),
		'layer must be provided and expected type is raster.'
	)

})
