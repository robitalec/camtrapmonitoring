context('test-dist-to')

data(water)
data(points)
data(densitygrid)

result <- dist_to(points, water)
inverse <- dist_to(water, points)
inside <- dist_to(points, densitygrid)


test_that('dist_to returns expected', {
	expect_equal(typeof(result), 'double')
	expect_equal(class(result), 'units')

	expect_equal(length(result), nrow(points))
	expect_equal(length(inverse), nrow(water))
	expect_equal(length(inside), nrow(points))
})


test_that('dist_to works from point to polygon', {
	expect_silent(dist_to(water, points))
})

test_that('dist_to works from polygon to point', {
	expect_silent(dist_to(points, water))

})

test_that('dist_to doesnt return negative values', {
	# since all points are within the densitygrid,
	# this should be all zeroes
	expect_true(all(result > units::set_units(0, m)))

	expect_true(all(inside == units::set_units(0, m)))
})



test_that('dist_to handles NAs', {
	expect_error(dist_to(x = NULL, y = points),
							 'please provide both x and y')

	expect_error(dist_to(x = points, y = NULL),
							 'please provide both x and y')

	expect_error(dist_to(x = NULL, y = NULL),
							 'please provide both x and y')
})