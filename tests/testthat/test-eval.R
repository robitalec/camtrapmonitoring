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
		'missing type and/or direction', fixed = FALSE
	)

	expect_warning(
		eval_pt(x = DT, layer = lc,
						type = 'categorical', direction = NULL,
						coords = c('X', 'Y')),
		'missing type and/or direction', fixed = FALSE
	)

	expect_error(
		eval_pt(x = DT, layer = lc,
						type = 'categorical', direction = 42,
						coords = c('X', 'Y')),
		'type and direction must be of class character'
	)

	expect_error(
		eval_pt(x = DT, layer = lc,
						type = 42, direction = 'positive',
						coords = c('X', 'Y')),
		'type and direction must be of class character'
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
	# nogeo <- data.frame(points$ID)
	# expect_error(
	# 	eval_pt(x = nogeo, layer = lc, type = 'categorical',
	# 					direction = 'neutral'),
	# 	'geometry column not found in x'
	# )


	lines <- st_cast(points, 'LINESTRING')

	expect_error(
		eval_pt(x = lines, layer = lc, type = 'categorical',
						direction = 'neutral'),
		'class of geometry column must be sfc_POINT'
	)
})


test_that("eval_buffer, general", {
	expect_error(
		eval_buffer(x = NULL, layer = lc, buffersize = 50,
								type = 'categorical', direction = 'neutral'),
		'x must be provided. either data.table or sf point object.'
	)

	expect_error(
		eval_buffer(x = points, layer = NULL, buffersize = 50,
								type = 'categorical', direction = 'neutral'),
		'layer must be provided. expected type is raster.'
	)

	expect_warning(
		eval_buffer(x = points, layer = lc, buffersize = 50,
								type = NULL, direction = 'neutral'),
		'missing type and/or direction', fixed = FALSE
	)

	expect_warning(
		eval_buffer(x = points, layer = lc, buffersize = 50,
								type = 'categorical', direction = NULL),
		'missing type and/or direction', fixed = FALSE
	)

	expect_error(
		eval_buffer(x = points, layer = lc, buffersize = 50,
								type = 'categorical', direction = 42),
		'type and direction must be of class character'
	)

	expect_error(
		eval_buffer(x = points, layer = lc, buffersize = 50,
								type = 42, direction = 'neutral'),
		'type and direction must be of class character'
	)

	expect_warning(
		eval_buffer(x = points, layer = lc, buffersize = 20,
								type = 'categorical', direction = 'neutral'),
		"buffersize is less than the layer's resolution"
	)
})


test_that("eval_buffer, data.table", {

	expect_error(
		eval_buffer(x = DT, layer = lc, buffersize = 50,
								type = 'categorical', direction = 'neutral',
								coords = 'X'),
		'length of coords column names should be 2'
	)


	DT[, xchr := 'potato']

	expect_error(
		eval_buffer(x = DT, layer = lc, buffersize = 50,
								type = 'categorical', direction = 'neutral',
								coords = c('xchr', 'xchr')),
		'coords provided must be numeric'
	)

})

test_that("eval_buffer, sf", {
	# nogeo <- data.frame(points$ID)
	# expect_error(eval_buffer(x = nogeo, layer = lc, buffersize = 50,
	# 												 type = 'categorical', direction = 'neutral'
	# 												 ), 'geometry column not found in x')


	multipoints <- st_cast(points, 'MULTIPOINT')

	expect_error(
		eval_buffer(x = multipoints, layer = lc, buffersize = 50,
								type = 'categorical', direction = 'neutral'),
		'class of geometry column must be sfc_POINT'
	)
})
