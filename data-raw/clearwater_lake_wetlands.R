# === Set up clearwater_lake_wetlands -------------------------------------



# Packages ----------------------------------------------------------------
library(osmdata)
library(sf)



# Extent ------------------------------------------------------------------
# Load Clearwater Lake extent
source('data-raw/clearwater_lake_extent.R')



# Wetlands ----------------------------------------------------------------
# Get wetland features from OSM
q <- opq(bbox = st_bbox(clearwater_lake_extent)) |>
	add_osm_feature(key = 'wetland') |>
	osmdata_sf()



# Reproject ---------------------------------------------------------------
crs <- st_crs(32614)
clearwater_lake_wetlands <- st_transform(q$osm_polygons, crs)



# Save --------------------------------------------------------------------
usethis::use_data(clearwater_lake_wetlands, overwrite = TRUE)
