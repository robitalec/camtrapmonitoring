context("test-select-ct")

# prep data.table
pts <-
	strat_sample(
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
	strat_sample(
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
	sfpt,
	n,
	rank = c('wetland'),
	sub = list(lc = 212),
	by = 'density'
)


# tests
test_that("select_works, general", {
	expect_error(select_ct(NULL, 10),
		'x is required. either a data.table or sf object.')

	expect_error(select_ct(pts, NULL), 'n is required.')

	expect_warning(
		select_ct(
			sfpt,
			n,
			rank = NULL,
			sub = NULL,
			by = NULL
		),
		'rank, sub and by are all NULL... selecting n rows  arbitrarily')

	expect_error(
		select_ct(
			sfpt,
			n,
			rank = c('lc')
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
