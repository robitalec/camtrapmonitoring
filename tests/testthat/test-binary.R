context("test-binary")

lc212 <- binary_layer(lc, 212)

test_that("binary_layer works", {

	expect_true(inherits(lc212, 'Raster'))

	expect_true(length(raster::unique(lc212)) <= 2)

	expect_equal(crs(lc), crs(lc212))

	expect_equal(res(lc), res(lc212))

	expect_error(binary_layer(),
							 'layer must be provided.')

	expect_error(binary_layer(lc),
							 'value must be provided.')

	expect_error(binary_layer('potato', 212),
							 'layer must be a raster.')

	expect_error(binary_layer(lc, 'potato'),
							 'value must be a numeric.')

	expect_error(binary_layer(lc, c(212, 212)),
							 'value must be of length one.')

})


test_that('binary_layer handles NA', {
	buf <- st_buffer(points[1,], 1e2)
	mlc <- mask(crop(lc, buf), buf, inverse = TRUE)

	expect_true(NA %in% raster::unique(mlc, na.last = TRUE))

	expect_true(NA %in% raster::unique(binary_layer(mlc, 212), na.last = TRUE))
})
