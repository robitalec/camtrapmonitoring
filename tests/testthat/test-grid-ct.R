context("test-grid-ct")

# Data
data("clearwater_lake_density")

# Sample points
points <- sample_ct(clearwater_lake_density, 15, type = 'random')


# make expected results
queen <- grid_ct(points, case = 'queen', distance = 100)
rook <- grid_ct(points, case = 'rook', distance = 100)
bishop <- grid_ct(points, case = 'bishop', distance = 100)

# tests
test_that("grid_ct's arguments are checked", {
	expect_error(
		grid_ct(features = points, distance = 100, case = 'potato'),
		'case provided must be one of "queen", "rook" or "bishop"'
	)

	expect_error(
		grid_ct(features = points, distance = -100, case = 'queen'),
		'distance must be a numeric, greater than 0'
	)

	expect_error(
		grid_ct(features = 1, distance = 100, case = 'queen'),
		'features are not class sf'
	)

	expect_error(
		grid_ct(features = points, n = 100, case = 'queen'),
		"argument \"distance\" is missing, with no default"
	)

	expect_error(
		grid_ct(features = points),
		"argument \"distance\" is missing, with no default"
	)

	multipoints <- st_cast(points, 'MULTIPOINT')

	expect_error(
		grid_ct(features = multipoints, distance = 100, case = 'queen'),
		'features are not geometry type POINT'
	)
})

test_that("grid_ct returns expected lengths", {
	expect_equal(
		nrow(grid_ct(features = points, distance = 100, n = 100)),
		nrow(points) * 100
	)

	expect_equal(
		nrow(grid_ct(features = points, distance = 100, case = 'queen')),
		nrow(points) * 9
	)

	expect_equal(
		nrow(grid_ct(features = points, distance = 100, case = 'rook')),
		nrow(points) * 5
	)

	expect_equal(
		nrow(grid_ct(features = points, distance = 100, case = 'bishop')),
		nrow(points) * 5
	)
})


test_that("grid_ct returns expected columns... for sf input", {
	expect_in(
		'id_grid_ct',
		colnames(queen)
	)
	expect_in(
		'id_grid_ct',
		colnames(rook)
	)
	expect_in(
		'id_grid_ct',
		colnames(bishop)
	)
	expect_in(
		'focal',
		colnames(queen)
	)
	expect_in(
		'focal',
		colnames(rook)
	)
	expect_in(
		'focal',
		colnames(bishop)
	)




	# camID match output length
	expect_equal(
		max(queen$id_grid_ct), nrow(points) * 9
	)

	expect_equal(
		max(rook$id_grid_ct), nrow(points) * 5
	)

	expect_equal(
		max(bishop$id_grid_ct), nrow(points) * 5
	)
})



# right focals