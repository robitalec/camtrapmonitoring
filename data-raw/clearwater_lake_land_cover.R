# === Set up clearwater_lake_land_cover -----------------------------------



# Packages ----------------------------------------------------------------
library(terra)



# Data source -------------------------------------------------------------
# 2020 Land Cover of Canada
# https://open.canada.ca/data/en/dataset/ee1580ab-a23d-4f86-a09b-79763677eb47
ca_lc <- rast('landcover-2020-classification.tif')



# Extent ------------------------------------------------------------------
# Load Clearwater Lake extent
source('data-raw/clearwater_lake_extent.R')

clearwater_extent_trans <- st_transform(clearwater_lake_extent, st_crs(ca_lc))



# Crop --------------------------------------------------------------------
ca_lc_crop <- crop(ca_lc, st_buffer(clearwater_extent_trans, 5e3))



# Reproject ---------------------------------------------------------------
crs <- st_crs(32614)
clearwater_lake_land_cover <- project(ca_lc_crop, crs$wkt, method = 'near')



# Save --------------------------------------------------------------------
# Save land cover data as external file in package
writeRaster(clearwater_lake_land_cover, file.path('inst', 'extdata', 'clearwater_lake_land_cover.tif'))
