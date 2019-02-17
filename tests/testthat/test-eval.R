context("test-eval")

library(data.table)
library(sf)

data(points)

DT <-
	data.table(ID = points$ID,
						 st_coordinates(points))



test_that("eval_pt, general", {
	expect_error(
		make_grid(points, case = 'potato', distance = 100),
		'must provide case one of "queen", "rook" or "bishop"'
	)

	expect_error(
		make_grid(points, case = 'queen', distance = -100),
		'layer must be provided. expected type is raster.'
	)

	expect_error(
		make_grid(1, case = 'queen', distance = 100),
		'no applicable method for ', fixed = FALSE
	)
})

test_that("eval_pt, data.table", {

})

test_that("eval_pt, sf", {

})


test_that("eval_buffer, general", {

})


test_that("eval_buffer, data.table", {

})

test_that("eval_buffer, sf", {

})
