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
})

test_that("with sf return", {
	pts <- strat_sample(densitygrid, 10,
											col = col, returnDT = FALSE)

	expect_equivalent(unique(pts[[col]]),
										unique(densitygrid[[col]]))
})