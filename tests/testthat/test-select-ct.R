context("test-select-ct")

# prep
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



# tests
test_that("select_works, general", {

})


test_that("select_works, data.table", {
	expect_true(inherits(sel, 'data.table'))

	expect_true(nrow(sel) <= (n * pts[, uniqueN(density)]))
})

test_that("select_works, sf", {

})
