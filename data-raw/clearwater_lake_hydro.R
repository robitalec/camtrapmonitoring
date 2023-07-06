# === Set up clearwater_lake_roads ---------------------------------------------



# Packages ----------------------------------------------------------------
library(osmdata)



# Extent ------------------------------------------------------------------
# Load Clearwater Lake extent
source('data-raw/clearwater_lake_extent.R')



# Major waterbodies -------------------------------------------------------
q <- opq(bbox = st_bbox(clearwater_lake_extent)) |>
	add_osm_feature(key = 'water')

clearwater_lake_hydro <- q$osm_multipolygons



# Save --------------------------------------------------------------------
usethis::use_data(clearwater_lake_hydro, overwrite = TRUE)
