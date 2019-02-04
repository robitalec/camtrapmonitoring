context("test-strat-sample")

data(densitygrid)

pts <- strat_sample(densitygrid, 10,
										col = 'density', returnDT = TRUE)

test_that("basic checks", {
	expect_equivalent(unique(pts$col),
										unique(densitygrid$density))
})
