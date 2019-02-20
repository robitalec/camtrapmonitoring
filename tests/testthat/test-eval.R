context("test-eval")

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
		eval_pt(x = DT, layer = lc,
						type = NULL, direction = 'neutral',
						coords = c('X', 'Y')),
		'missing type and', fixed = FALSE
	)

	expect_warning(
		eval_pt(x = DT, layer = lc,
						type = 'categorical', direction = NULL,
						coords = c('X', 'Y')),
		'missing type and/or direction', fixed = FALSE
	)

})

test_that("eval_pt, data.table", {

	expect_error(
		eval_pt(x = DT, layer = lc, type = 'categorical',
						direction = 'neutral', coords = 'X'),
		'length of coords column names should be 2'
	)


	DT[, xchr := 'potato']

	expect_error(
		eval_pt(x = DT, layer = lc, type = 'categorical',
						direction = 'neutral', coords = c('xchr', 'xchr')),
		'coords provided must be numeric'
	)

})

test_that("eval_pt, sf", {
	multipoints <- st_cast(points, 'MULTIPOINT')

	expect_error(
		eval_pt(x = multipoints, layer = lc, type = 'categorical',
						direction = 'neutral'),
		'class of geometry column must be sfc_POINT'
	)
})


# test_that("eval_buffer, general", {
#
# })


# test_that("eval_buffer, data.table", {
#
# })

# test_that("eval_buffer, sf", {
#
# })
