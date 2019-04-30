context('test-dist-to')

result <- eval_dist(points, water)
inverse <- eval_dist(water, points)
inside <- eval_dist(points, densitygrid)


test_that('eval_dist returns expected', {
	expect_equal(typeof(result), 'double')
	expect_equal(class(result), 'units')

	expect_equal(length(result), nrow(points))
	expect_equal(length(inverse), nrow(water))
	expect_equal(length(inside), nrow(points))
})


test_that('eval_dist works from point to polygon', {
	expect_silent(eval_dist(water, points))
})

test_that('eval_dist works from polygon to point', {
	expect_silent(eval_dist(points, water))

})

test_that('eval_dist doesnt return negative values', {
	# since all points are within the densitygrid,
	# this should be all zeroes
	expect_true(all(result > units::set_units(0, m)))

	expect_true(all(inside == units::set_units(0, m)))
})



test_that('eval_dist handles NAs', {
	expect_error(eval_dist(x = NULL, y = points),
							 'please provide both x and y')

	expect_error(eval_dist(x = points, y = NULL),
							 'please provide both x and y')

	expect_error(eval_dist(x = NULL, y = NULL),
							 'please provide both x and y')
})