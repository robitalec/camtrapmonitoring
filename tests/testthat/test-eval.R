context("test-eval")

# Packages
library(terra)
library(sf)

# Data
data("clearwater_lake_density")
data("clearwater_lake_hydro")
clearwater_lake_land_cover <- rast(system.file('extdata', 'clearwater_lake_land_cover.tif', package = 'camtrapmonitoring'))

clearwater_lake_elevation_path <- system.file('extdata', 'clearwater_lake_elevation.tif', package = 'camtrapmonitoring')
clearwater_lake_elevation <- rast(clearwater_lake_elevation_path)

# Sample points
points <- sample_ct(region = clearwater_lake_density, n = 15, type = 'random')

# Evaluate each point with the land cover layer
points$lc <- eval_pt(features = points, target = clearwater_lake_land_cover)

test_that("eval_pt's arguments are checked", {
	expect_error(
		eval_pt(features = points, target = NULL),
		'target must be provided'
	)

	expect_error(
		eval_pt(features = NULL, target = clearwater_lake_land_cover),
		'features must be provided'
	)

	lines <- st_cast(points, 'LINESTRING')

	expect_error(
		eval_pt(features = lines, target = clearwater_lake_land_cover),
		'features is not of geometry type POINT'
	)
})

test_that("eval_pt returns numeric when expected", {
	expect_type(
		eval_pt(features = points, target = clearwater_lake_elevation),
		'numeric'
	)
})


test_that("eval_buffer's arguments are checked", {
	expect_error(
		eval_buffer(features = points, target = NULL, buffer_size = 20),
		'target must be provided'
	)

	expect_error(
		eval_buffer(features = NULL, target = clearwater_lake_land_cover,  buffer_size = 20),
		'features must be provided'
	)

	multipoints <- st_cast(points, 'MULTIPOINT')

	# expect_error(
	# 	eval_buffer(features = multipoints, target = clearwater_lake_land_cover,
	# 							buffer_size = 50),
	# 	'features is not of geometry type POINT'
	# )
})

test_that("eval_buffer returns numeric when expected", {
	expect_type(
		eval_buffer(features = points, target = clearwater_lake_elevation),
		'numeric'
	)
})


## eval_dist
test_that("eval_dist's arguments are checked", {
	result <- eval_dist(points, clearwater_lake_hydro)
	expect_equal(typeof(result), 'double')

	expect_equal(length(result), nrow(points))

	expect_error(eval_dist(features = points, target = NULL),
							 'please provide target')

	expect_error(eval_dist(features = NULL, target = points),
							 'please provide features')

})


test_that('eval_dist works from point to polygon', {
	expect_silent(eval_dist(points, clearwater_lake_hydro))
})


test_that('eval_dist doesnt return negative values', {
	expect_true(all(eval_dist(points, clearwater_lake_hydro) >= 0))
})
