# === Set up rocky_lake_roads ---------------------------------------------



# Packages ----------------------------------------------------------------
library(sf)



# Data source -------------------------------------------------------------
# https://open.canada.ca/data/en/dataset/3d282116-e556-400c-9306-ca1a3cada77f
# National Road Network - NRN - GeoBase Series - NRN Manitoba GEOPACKAGE
mb_roads <- st_read('nrn_rrn_mb_GPKG/NRN_MB_6_0_GPKG_en.gpkg',
										layer = 'NRN_MB_6_0_ROADSEG')



# Extent ------------------------------------------------------------------
mb_extent <- st_as_sf(data.frame(x = c(-101.72,-100.94), y = c(53.91, 54.37)),
											coords = c('x', 'y'), crs = 4326)

mb_bbox_roads <- st_bbox(st_transform(mb_extent, st_crs(mb_roads)))



# Crop --------------------------------------------------------------------
rocky_lake_roads <- st_crop(mb_roads, mb_bbox_roads)



# Save --------------------------------------------------------------------
usethis::use_data(rocky_lake_roads, overwrite = TRUE)
