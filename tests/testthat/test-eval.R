context("test-eval")

library(data.table)
library(sf)

data(points)
data(lc)

DT <-
	data.table(ID = points$ID,
						 st_coordinates(points))

###
expect_error(
	eval_pt(x = DT, layer = lc,
					type = 'categorical', direction = 'neutral',
					coords = c('X', 'Y')),
)
###

test_that("eval_pt, general", {
	expect_error(
		eval_pt(x = NULL, layer = lc,
						type = 'categorical', direction = 'neutral',
						coords = c('X', 'Y')),
		'x must be provided. either data.table or sf point object.'
	)

	expect_error(
		eval_pt(x = DT, layer = NULL,
						type = 'categorical', direction = 'neutral',
						coords = c('X', 'Y')),
		'layer must be provided. expected type is raster.'
	)

	expect_warning(
		eval_pt(x = DT, layer = NULL,
						type = NULL, direction = 'neutral',
						coords = c('X', 'Y')),
		'missing type and/or direction', fixed = FALSE
	)

	expect_warning(
		eval_pt(x = DT, layer = NULL,
						type = 'categorical', direction = NULL,
						coords = c('X', 'Y')),
		'missing type and/or direction', fixed = FALSE
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
