context("test-make-grid")

# make expected results
queen <- make_grid(points, case = 'queen', distance = 100)
rook <- make_grid(points, case = 'rook', distance = 100)
bishop <- make_grid(points, case = 'bishop', distance = 100)

gridQ <- make_grid(DT, case = 'queen', distance = 100,
									 id = 'ID', coords = c('X', 'Y'))
gridR <- make_grid(DT, case = 'rook', distance = 100,
									 id = 'ID', coords = c('X', 'Y'))
gridB <- make_grid(DT, case = 'bishop', distance = 100,
									 id = 'ID', coords = c('X', 'Y'))

# tests
test_that("make_grid works", {
	expect_error(
		make_grid(points, case = 'potato', distance = 100),
		'must provide case one of "queen", "rook" or "bishop"'
	)

	expect_error(
		make_grid(points, case = 'queen', distance = -100),
		'distance must be a numeric, greater than 0'
	)

	expect_error(
		make_grid(1, case = 'queen', distance = 100),
		'no applicable method for ', fixed = FALSE
	)

})


test_that("... for data.table input", {
	expect_equal(
		ncol(DT) + 1, ncol(gridQ)
	)

	expect_true(
		'focal' %in% colnames(gridQ)
	)

	expect_error(
		make_grid(DT, case = 'queen', distance = 100, id = 'ID',
							coords = NULL),
		'id and coords must be provided with x is a data.table'
	)

	expect_error(
		make_grid(DT, case = 'queen', distance = 100, id = NULL,
							coords = c('X', 'Y')),
		'id and coords must be provided with x is a data.table'
	)

	expect_error(
		make_grid(DT, case = 'queen', distance = 100, id = 'potato',
							coords = c('X', 'Y')),
		'id provided not found in colnames', fixed = FALSE
	)


	expect_error(
		make_grid(DT, case = 'queen', distance = 100, id = 'ID',
							coords = c('potatoX', 'Y')),
		'coords provided not found in colnames', fixed = FALSE
	)

	expect_equal(
		nrow(gridQ), nrow(DT) * 9
	)

	expect_equal(
		nrow(gridR), nrow(DT) * 5
	)

	expect_equal(
		nrow(gridB), nrow(DT) * 5
	)

})


test_that("... for sf input", {
	multipoints <- st_cast(points, 'MULTIPOINT')

	expect_error(
		make_grid(multipoints, 'queen', 100),
		'class of geometry column must be sfc_POINT'
	)

	expect_equal(
		nrow(queen), nrow(DT) * 9
	)

	expect_equal(
		nrow(rook), nrow(DT) * 5
	)

	expect_equal(
		nrow(bishop), nrow(DT) * 5
	)
})
