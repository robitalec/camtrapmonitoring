context("test-strat-sample")

# make expected results
col <- 'density'
pts <- sample_ct(densitygrid, 10, type = 'random',
										col = col, returnDT = TRUE)

# tests
test_that("basics", {
	expect_error(sample_ct(densitygrid, 10, type = 'random', col = 'potato'),
							 error = 'strata column not found in x')

	densitygrid$potato <- NA
	expect_error(sample_ct(densitygrid, 10, type = 'random', col = 'potato'),
							 error = 'no strata found')

	expect_error(sample_ct(densitygrid, 10, col = col),
							 error = 'type must be provided. either "regular" or "random"')

})

test_that("with data.table return", {
	pts <- sample_ct(densitygrid, 10, type = 'random',
											col = col, returnDT = TRUE)

	expect_equivalent(unique(pts[[col]]),
										unique(densitygrid[[col]]))


	expect_true(inherits(pts, 'data.table'))

})

test_that("with sf return", {
	pts <- sample_ct(densitygrid, 10, type = 'random',
											col = col, returnDT = FALSE)

	expect_equivalent(unique(pts[[col]]),
										unique(densitygrid[[col]]))

	expect_true(inherits(pts, 'sf'))
})


test_that("type of col is well handled", {
	# factor
	densitygrid$tryfactor <- as.factor(densitygrid$density)
	pts <- sample_ct(densitygrid, 10, type = 'random',
											col = 'tryfactor', returnDT = FALSE)

	expect_equivalent(unique(pts[['tryfactor']]),
										unique(densitygrid[['tryfactor']]))

	# numeric
	densitygrid$trynum <- sample(1:3, size = 4, replace = TRUE)
	pts <- sample_ct(densitygrid, 10, type = 'random',
											col = 'trynum', returnDT = FALSE)

	expect_equivalent(unique(pts[['trynum']]),
										unique(densitygrid[['trynum']]))

	# boolean
	densitygrid$trybool <- sample(c(TRUE, FALSE), size = 4, replace = TRUE)
	pts <- sample_ct(densitygrid, 10, type = 'random',
											col = 'trybool', returnDT = FALSE)

	expect_equivalent(unique(pts[['trybool']]),
										unique(densitygrid[['trybool']]))

})