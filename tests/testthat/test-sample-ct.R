# Context -----------------------------------------------------------------
context("test-sample")



# Data --------------------------------------------------------------------
data("clearwater_lake_density")
strata <- 'density'



# Tests -------------------------------------------------------------------
test_that("checks error as expected", {
	expect_error(sample_ct(clearwater_lake_density, 10,
												 type = 'random', strata = 'potato'),
							 error = 'strata column not found in region')

	clearwater_lake_density$potato <- NA
	expect_error(sample_ct(clearwater_lake_density, 10, type = 'random', strata = 'potato'),
							 error = 'no strata found')

	expect_error(sample_ct(clearwater_lake_density, 10, strata = strata),
							 error = 'type must be provided. either "regular" or "random"')
})



test_that("levels returned in output match input", {
	strata <- 'density'
	pts <- sample_ct(region = clearwater_lake_density, n = 20, type = 'regular',
									 strata = strata)

	expect_equivalent(unique(pts[[strata]]),
										unique(clearwater_lake_density[[strata]]))
})



test_that("returns sf object", {
	pts <- sample_ct(clearwater_lake_density, 10, type = 'random',
											strata = strata)

	expect_s3_class(pts, 'sf')
})



test_that("type of strata is well handled", {
	# factor
	clearwater_lake_density$tryfactor <- as.factor(clearwater_lake_density$density)
	pts <- sample_ct(clearwater_lake_density, 10, type = 'random',
											strata = 'tryfactor')

	expect_equivalent(unique(pts[['tryfactor']]),
										unique(clearwater_lake_density[['tryfactor']]))

	# numeric
	clearwater_lake_density$trynum <- sample(1:3, size = 4, replace = TRUE)
	pts <- sample_ct(clearwater_lake_density, 10, type = 'random',
											strata = 'trynum')

	expect_equivalent(unique(pts[['trynum']]),
										unique(clearwater_lake_density[['trynum']]))

	# boolean
	clearwater_lake_density$trybool <- sample(c(TRUE, FALSE), size = 4, replace = TRUE)
	pts <- sample_ct(clearwater_lake_density, 10, type = 'random',
											strata = 'trybool')

	expect_equivalent(unique(pts[['trybool']]),
										unique(clearwater_lake_density[['trybool']]))

})
