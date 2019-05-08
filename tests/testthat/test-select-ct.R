context("test-select-ct")

# prep data.table
pts <-
	sample_ct(
		densitygrid,
		n = 5,
		type = 'random',
		col = 'density',
		returnDT = TRUE
	)

pts[, lc := eval_pt(
	.SD,
	lc,
	type = 'categorical',
	direction = 'neutral',
	coords = c('X', 'Y')
)]
pts[, dem := eval_buffer(
	.SD,
	dem,
	buffersize = 100,
	type = 'real',
	direction = 'positive',
	coords = c('X', 'Y')
)]
pts[, wetland := eval_buffer(
	.SD,
	wetland,
	100,
	'binary',
	'negative',
	coords = c('X', 'Y'))]

n <- 1

sel <- select_ct(
	pts,
	n,
	rank = c('wetland'),
	sub = list(lc = 212),
	by = 'density'
)

# prep sf
sfpt <-
	sample_ct(
		densitygrid,
		n = 5,
		type = 'random',
		col = 'density')

sfpt$lc <- eval_pt(
	sfpt,
	lc,
	type = 'categorical',
	direction = 'neutral'
)
sfpt$dem <- eval_buffer(
	sfpt,
	dem,
	buffersize = 100,
	type = 'real',
	direction = 'positive'
)
sfpt$wetland <- eval_buffer(
	sfpt,
	wetland,
	100,
	'binary',
	'negative'
)

n <- 1

sfsel <- select_ct(
	x = sfpt,
	n = n,
	rank = c('wetland'),
	sub = list(lc = 212),
	by = 'density'
)


# tests
test_that("select works, general", {
	expect_error(select_ct(),
		'x is required. either a data.table or sf object.')

	expect_error(select_ct(pts),
							 'n is required.')

	expect_error(select_ct(pts, -1),
							 'n must be a positive numeric.')

	expect_error(select_ct(pts, 'potato'),
							 'n must be a positive numeric.')


	expect_warning(
		select_ct(
			x = sfpt,
			n = n,
			rank = NULL,
			sub = NULL,
			by = NULL
		),
		'rank, sub and by are all NULL... selecting first n rows arbitrarily')

	expect_error(
		select_ct(
			sfpt,
			n,
			rank = 'lc'
		),
		'cannot rank columns with a neutral direction'
	)
})


test_that("select_works, data.table", {
	expect_true(inherits(sel, 'data.table'))

	expect_true(nrow(sel) <= (n * pts[, uniqueN(density)]))
})

test_that("select_works, sf", {
	expect_true(inherits(sfsel, 'sf'))

	expect_true(nrow(sfsel) <= (n * length(unique(pts$density))))
})


test_that("rank works", {
	# expect_true()
	# expect that rank with length 2 works

	# expect that rank with no direction returns error

	pts[, potato := 'potato']

	expect_error(
		select_ct(
			pts,
			n,
			rank = c('potato'),
			sub = list(lc = 212),
			by = 'density'
		),
		'columns in rank do not have direction attribute, did you use eval_*?'
	)

})


test_that("sub can be null", {
	expect_silent(select_ct(
		x = pts,
		n = n,
		rank = c('wetland'),
		# sub = list(lc = 212),
		by = 'density'
	))

	expect_silent(select_ct(
		x = pts,
		n = n,
		rank = c('wetland'),
		sub = NULL,
		by = 'density'
	))

})


test_that("by can be greater than length 1", {
	# If n is 1, unique in pts should be the same as sel
	n <- 1

	sel <-
		select_ct(
			x = pts,
			n = n,
			rank = c('wetland'),
			by = c('lc', 'density')
		)
	expect_equal(sel[, .(lc, density)],
							 unique(pts[, .(lc, density)]))


	# Fake another by column
	pts[, potato := rep(c('russet', 'yukongold'), length.out = .N)]

	n <- 1
	sel <- select_ct(
		x = pts,
		n = n,
		rank = c('wetland'),
		sub = list(lc = 212),
		by = c('potato', 'density')
	)

	expect_equal(
		uniqueN(sel[, .(potato, density)]),
		uniqueN(pts[lc == 212, .(potato, density)])
	)


})
})