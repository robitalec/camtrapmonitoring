context("test-eval")

# Packages
library(terra)
library(sf)

# Data
data("clearwater_lake_density")
data("clearwater_lake_hydro")
clearwater_lake_land_cover <- rast(system.file('extdata', 'clearwater_lake_land_cover.tif', package = 'camtrapmonitoring'))

# Sample points
points <- sample_ct(clearwater_lake_density, 15, type = 'random')

# Evaluate each point with the land cover layer
points$lc <- eval_pt(x = clearwater_lake_land_cover, y = points)

test_that("eval_pt's arguments are checked", {
	expect_error(
		eval_pt(x = NULL, y = points),
		'x must be provided'
	)

	expect_error(
		eval_pt(x = clearwater_lake_land_cover, y = NULL),
		'y must be provided'
	)

	lines <- st_cast(points, 'LINESTRING')

	expect_error(
		eval_pt(x = clearwater_lake_land_cover, y = lines),
		'y is not of geometry type POINT'
	)
})


test_that("eval_buffer's arguments are checked", {
	expect_error(
		eval_buffer(x = NULL, y = points, buffer_size = 20),
		'x must be provided'
	)

	expect_error(
		eval_buffer(x = clearwater_lake_land_cover, y = NULL, buffer_size = 20),
		'y must be provided'
	)

	multipoints <- st_cast(points, 'MULTIPOINT')

	expect_error(
		eval_buffer(x = clearwater_lake_land_cover, y = multipoints, buffer_size = 50),
		'y is not of geometry type POINT'
	)
})



## eval_dist
test_that("eval_dist's arguments are checked", {
	result <- eval_dist(clearwater_lake_hydro, points)
	expect_equal(typeof(result), 'double')

	expect_equal(length(result), nrow(points))
	expect_equal(length(inverse), nrow(water))
	expect_equal(length(inside), nrow(points))

	expect_error(eval_dist(x = NULL, y = points),
							 'please provide both x and y')

	expect_error(eval_dist(x = points, y = NULL),
							 'please provide both x and y')

	expect_error(eval_dist(x = NULL, y = NULL),
							 'please provide both x and y')
})


test_that('eval_dist works from point to polygon', {
	expect_silent(eval_dist(clearwater_lake_hydro, points))
})


test_that('eval_dist doesnt return negative values', {
	expect_true(all(result >= 0))
})