# === Set up clearwater_lake_hydro ----------------------------------------



# Packages ----------------------------------------------------------------
library(osmdata)
library(sf)



# Extent ------------------------------------------------------------------
# Load Clearwater Lake extent
source('data-raw/clearwater_lake_extent.R')



# Hydro -------------------------------------------------------------------
# Get water features from OSM
q <- opq(bbox = st_bbox(clearwater_lake_extent)) |>
	add_osm_feature(key = 'water')

# Select only multipolygons features (larger waterbodies in this case)
clearwater_lake_hydro <- q$osm_multipolygons



# Save --------------------------------------------------------------------
usethis::use_data(clearwater_lake_hydro, overwrite = TRUE)