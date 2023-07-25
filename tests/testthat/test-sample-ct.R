# Context -----------------------------------------------------------------
context("test-sample")



# Data --------------------------------------------------------------------
data("clearwater_lake_density")
col <- 'density'



# Tests -------------------------------------------------------------------
test_that("checks error as expected", {
	expect_error(sample_ct(clearwater_lake_density, 10, type = 'random', col = 'potato'),
							 error = 'strata column not found in x')

	clearwater_lake_density$potato <- NA
	expect_error(sample_ct(clearwater_lake_density, 10, type = 'random', col = 'potato'),
							 error = 'no strata found')

	expect_error(sample_ct(clearwater_lake_density, 10, col = col),
							 error = 'type must be provided. either "regular" or "random"')
})



test_that("levels returned in output match input", {
	col <- 'density'
	pts <- sample_ct(x = clearwater_lake_density, n = 20, type = 'regular',
									 col = col)

	expect_equivalent(unique(pts[[col]]),
										unique(clearwater_lake_density[[col]]))
})



test_that("returns sf object", {
	pts <- sample_ct(clearwater_lake_density, 10, type = 'random',
											col = col)

	expect_s3_class(pts, 'sf')
})



test_that("type of col is well handled", {
	# factor
	clearwater_lake_density$tryfactor <- as.factor(clearwater_lake_density$density)
	pts <- sample_ct(clearwater_lake_density, 10, type = 'random',
											col = 'tryfactor')

	expect_equivalent(unique(pts[['tryfactor']]),
										unique(clearwater_lake_density[['tryfactor']]))

	# numeric
	clearwater_lake_density$trynum <- sample(1:3, size = 4, replace = TRUE)
	pts <- sample_ct(clearwater_lake_density, 10, type = 'random',
											col = 'trynum')

	expect_equivalent(unique(pts[['trynum']]),
										unique(clearwater_lake_density[['trynum']]))

	# boolean
	clearwater_lake_density$trybool <- sample(c(TRUE, FALSE), size = 4, replace = TRUE)
	pts <- sample_ct(clearwater_lake_density, 10, type = 'random',
											col = 'trybool')

	expect_equivalent(unique(pts[['trybool']]),
										unique(clearwater_lake_density[['trybool']]))

})
