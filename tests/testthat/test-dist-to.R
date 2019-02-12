context("test-dist-to")

data(water)
data(points)
data(densitygrid)

result <- dist_to(points, water)
inverse <- dist_to(water, points)
inside <- dist_to(points, densitygrid)


test_that("dist_to returns expected", {
	expect_equal(typeof(result), 'double')
	expect_equal(class(result), 'units')
})


test_that("dist_to works from point to polygon", {
	expect_silent(dist_to(water, points))
})

test_that("dist_to works from polygon to point", {
	expect_silent(dist_to(points, water))

})

test_that("dist_to doesn't return negative values", {
	# since all points are within the densitygrid,
	# this should be all zeroes
	expect_gt(inside, 0)

	expect_true(all(inside == units::set_units(0, m)))
})