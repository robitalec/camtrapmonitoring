# === Set up clearwater_lake_extent --------------------------------------------



# Packages ----------------------------------------------------------------
library(sf)



# Extent ------------------------------------------------------------------
clearwater_lake_extent <- st_as_sf(
	data.frame(x = c(-101.38, -100.88), y = c(53.90, 54.18)),
	coords = c('x', 'y'),
	crs = 4326
)



# Save --------------------------------------------------------------------
usethis::use_data(clearwater_lake_extent, overwrite = TRUE)
