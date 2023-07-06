# === Set up clearwater_lake_roads ----------------------------------------



# Packages ----------------------------------------------------------------
library(sf)



# Data source -------------------------------------------------------------
# National Road Network - NRN - GeoBase Series - NRN Manitoba GEOPACKAGE
# https://open.canada.ca/data/en/dataset/3d282116-e556-400c-9306-ca1a3cada77f
mb_roads <- st_read('nrn_rrn_mb_GPKG/NRN_MB_6_0_GPKG_en.gpkg',
										layer = 'NRN_MB_6_0_ROADSEG')



# Extent ------------------------------------------------------------------
# Load Clearwater Lake extent
source('data-raw/clearwater_lake_extent.R')

clearwater_bbox <- st_bbox(st_transform(clearwater_lake_extent, st_crs(mb_roads)))



# Crop --------------------------------------------------------------------
clearwater_roads_crop <- st_crop(mb_roads, clearwater_bbox)



# Reproject ---------------------------------------------------------------
crs <- st_crs(32614)
clearwater_lake_roads <- st_transform(clearwater_roads_crop, crs)



# Save --------------------------------------------------------------------
usethis::use_data(clearwater_lake_roads, overwrite = TRUE)
