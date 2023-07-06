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
	add_osm_feature(key = 'water') |>
	osmdata_sf()

# Select only multipolygons features (larger waterbodies in this case)
clearwater_lake_hydro <- q$osm_multipolygons



# Reproject ---------------------------------------------------------------
crs <- st_crs(32614)
clearwater_lake_hydro <- st_transform(clearwater_lake_hydro, crs)



# Save --------------------------------------------------------------------
# Save hydro data as R object in package
usethis::use_data(clearwater_lake_hydro, overwrite = TRUE)
