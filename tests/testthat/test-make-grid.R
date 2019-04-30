context("test-make-grid")

# make expected results
queen <- grid_ct(points, case = 'queen', distance = 100)
rook <- grid_ct(points, case = 'rook', distance = 100)
bishop <- grid_ct(points, case = 'bishop', distance = 100)

gridQ <- grid_ct(DT, case = 'queen', distance = 100,
									 id = 'ID', coords = c('X', 'Y'))
gridR <- grid_ct(DT, case = 'rook', distance = 100,
									 id = 'ID', coords = c('X', 'Y'))
gridB <- grid_ct(DT, case = 'bishop', distance = 100,
									 id = 'ID', coords = c('X', 'Y'))

# tests
test_that("grid_ct works", {
	expect_error(
		grid_ct(points, case = 'potato', distance = 100),
		'case provided must be one of "queen", "rook" or "bishop"'
	)

	expect_error(
		grid_ct(points, case = 'queen', distance = -100),
		'distance must be a numeric, greater than 0'
	)

	expect_error(
		grid_ct(1, case = 'queen', distance = 100),
		'no applicable method for ', fixed = FALSE
	)

	expect_error(
		grid_ct(points, n = 100, case = 'queen'),
		'provide one of n and case and not both.'
	)

	expect_error(
		grid_ct(points),
		'provide one of n and case and not both.'
	)

	expect_equal(
		nrow(grid_ct(points, n = 100, distance = 100)),
		nrow(points) * 100
	)

	expect_equal(
		nrow(grid_ct(points, case = 'queen', distance = 100)),
		nrow(points) * 9
	)

	expect_equal(
		nrow(grid_ct(points, case = 'rook', distance = 100)),
		nrow(points) * 5
	)

	expect_equal(
		nrow(grid_ct(points, case = 'bishop', distance = 100)),
		nrow(points) * 5
	)


})


test_that("... for data.table input", {
	expect_equal(
		ncol(DT) + 2, ncol(gridQ)
	)

	expect_true(
		'camID' %in% colnames(gridQ)
	)
	expect_true(
		'camID' %in% colnames(gridR)
	)
	expect_true(
		'camID' %in% colnames(gridB)
	)

	expect_true(
		'focal' %in% colnames(gridQ)
	)
	expect_true(
		'focal' %in% colnames(gridR)
	)
	expect_true(
		'focal' %in% colnames(gridB)
	)

	expect_error(
		grid_ct(DT, case = 'queen', distance = 100, id = 'ID',
							coords = NULL),
		'id and coords must be provided with x is a data.table'
	)

	expect_error(
		grid_ct(DT, case = 'queen', distance = 100, id = NULL,
							coords = c('X', 'Y')),
		'id and coords must be provided with x is a data.table'
	)

	expect_error(
		grid_ct(DT, case = 'queen', distance = 100, id = 'potato',
							coords = c('X', 'Y')),
		'id provided not found in colnames', fixed = FALSE
	)


	expect_error(
		grid_ct(DT, case = 'queen', distance = 100, id = 'ID',
							coords = c('potatoX', 'Y')),
		'coords provided not found in colnames', fixed = FALSE
	)

	# Output rows match expected length
	expect_equal(
		nrow(gridQ), nrow(DT) * 9
	)

	expect_equal(
		nrow(gridR), nrow(DT) * 5
	)

	expect_equal(
		nrow(gridB), nrow(DT) * 5
	)

	# camID match output length
	expect_equal(
		max(gridQ$camID), nrow(DT) * 9
	)

	expect_equal(
		max(gridR$camID), nrow(DT) * 5
	)

	expect_equal(
		max(gridB$camID), nrow(DT) * 5
	)

})


test_that("... for sf input", {
	# nogeo <- data.frame(points$ID)
	# expect_error(
	# 	grid_ct(nogeo, 'queen', 100),
	# 	'geometry column not found in x'
	# )

	expect_true(
		'camID' %in% colnames(queen)
	)
	expect_true(
		'camID' %in% colnames(rook)
	)
	expect_true(
		'camID' %in% colnames(bishop)
	)

	expect_true(
		'focal' %in% colnames(queen)
	)
	expect_true(
		'focal' %in% colnames(rook)
	)
	expect_true(
		'focal' %in% colnames(bishop)
	)


	multipoints <- st_cast(points, 'MULTIPOINT')

	expect_error(
		grid_ct(multipoints, case = 'queen', distance = 100),
		'class of geometry column must be sfc_POINT'
	)

	expect_equal(
		nrow(queen), nrow(points) * 9
	)

	expect_equal(
		nrow(rook), nrow(points) * 5
	)

	expect_equal(
		nrow(bishop), nrow(points) * 5
	)

	# camID match output length
	expect_equal(
		max(queen$camID), nrow(points) * 9
	)

	expect_equal(
		max(rook$camID), nrow(points) * 5
	)

	expect_equal(
		max(bishop$camID), nrow(points) * 5
	)
})



# right focals