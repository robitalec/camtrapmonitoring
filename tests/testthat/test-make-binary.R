context("test-make-binary")

lc212 <- make_binary(lc, 212)

test_that("make_binary works", {

	expect_true(inherits(lc212, 'Raster'))

	expect_true(length(raster::unique(lc212)) <= 2)

	expect_equal(crs(lc), crs(lc212))

	expect_equal(res(lc), res(lc212))


	expect_error(make_binary(),
							 'layer must be provided.')

	expect_error(make_binary(lc),
							 'value must be provided.')

	expect_error(make_binary('potato', 212),
							 'layer must be a raster.')

	expect_error(make_binary(lc, 'potato'),
							 'value must be a numeric.')

	expect_error(make_binary(lc, c(212, 212)),
							 'value must be of length one.')

})