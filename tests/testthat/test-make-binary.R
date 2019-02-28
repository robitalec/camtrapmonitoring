context("test-make-binary")

lc212 <- make_binary(lc, 212)

test_that("make_binary works", {

	expect_true(inherits(lc212, 'Raster'))

	expect_true(unique(lc212) <= 2)

	expect_equal(crs(lc), crs(lc212))

	expect_equal(res(lc), res(lc212))

})
