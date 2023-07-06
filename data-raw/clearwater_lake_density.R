# === Set up clearwater_lake_density --------------------------------------



# Packages ----------------------------------------------------------------
library(sf)



# Extent ------------------------------------------------------------------
# Load Clearwater Lake extent
source('data-raw/clearwater_lake_extent.R')

crs <- st_crs(32614)
clearwater_extent_trans <- st_transform(clearwater_lake_extent, crs)


# Density -----------------------------------------------------------------
# Make grid
clearwater_lake_density <- st_make_grid(clearwater_extent_trans, cellsize = 2e3) |>
	st_as_sf()

# Simulate some species density
clearwater_lake_density$density <- sample(
	factor(c(3, 2, 1), labels = c('High', 'Medium', 'Low')),
	size = nrow(clearwater_lake_grid),
	replace = TRUE,
	prob = c(0.1, 0.3, 0.6)
)



# Save --------------------------------------------------------------------
usethis::use_data(clearwater_lake_density, overwrite = TRUE)
