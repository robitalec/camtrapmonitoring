context("test-strat-sample")

data(densitygrid)
col <- 'density'
pts <- strat_sample(densitygrid, 10, col = col, returnDT = TRUE)


test_that("basics", {
	expect_error(strat_sample(densitygrid, 10, col = 'potato'),
							 error = 'strata column not found in x')

	densitygrid$potato <- NA
	expect_error(strat_sample(densitygrid, 10, col = 'potato'),
							 error = 'no strata found')
})



test_that("with data.table return", {
	pts <- strat_sample(densitygrid, 10,
											col = col, returnDT = TRUE)

	expect_equivalent(unique(pts[[col]]),
										unique(densitygrid[[col]]))


	expect_true('data.table' %in% class(pts))

})

test_that("with sf return", {
	pts <- strat_sample(densitygrid, 10,
											col = col, returnDT = FALSE)

	expect_equivalent(unique(pts[[col]]),
										unique(densitygrid[[col]]))

	expect_true('sf' %in% class(pts))
})


test_that("type of col is well handled", {
	# factor
	densitygrid$tryfactor <- as.factor(densitygrid$density)
	pts <- strat_sample(densitygrid, 10,
											col = 'tryfactor', returnDT = FALSE)

	expect_equivalent(unique(pts[['tryfactor']]),
										unique(densitygrid[['tryfactor']]))

	# numeric
	densitygrid$trynum <- sample(1:3, size = 4, replace = TRUE)
	pts <- strat_sample(densitygrid, 10,
											col = 'trynum', returnDT = FALSE)

	expect_equivalent(unique(pts[['trynum']]),
										unique(densitygrid[['trynum']]))

	# boolean
	densitygrid$trybool <- sample(c(TRUE, FALSE), size = 4, replace = TRUE)
	pts <- strat_sample(densitygrid, 10,
											col = 'trybool', returnDT = FALSE)

	expect_equivalent(unique(pts[['trybool']]),
										unique(densitygrid[['trybool']]))

})