context("test-strat-sample")

data(densitygrid)

pts <- strat_sample(densitygrid, 10,
										col = 'density', returnDT = TRUE)

test_that("basic checks with data.table return", {
	pts <- strat_sample(densitygrid, 10,
											col = 'density', returnDT = TRUE)

	expect_equivalent(unique(pts$col),
										unique(densitygrid$density))
})

test_that("basic checks with sf return", {
	pts <- strat_sample(densitygrid, 10,
											col = 'density', returnDT = FALSE)

	expect_equivalent(unique(pts$col),
										unique(densitygrid$density))
})