# === Set up clearwater_lake_elevation ------------------------------------



# Packages ----------------------------------------------------------------
library(elevatr)
library(sf)
library(terra)



# Extent ------------------------------------------------------------------
# Load Clearwater Lake extent
source('data-raw/clearwater_lake_extent.R')

# Reproject extent
crs <- st_crs(32614)
clearwater_extent_trans <- st_transform(clearwater_lake_extent, crs)



# Elevation --------------------------------------------------------------
clearwater_lake_elevation <- get_elev_raster(clearwater_extent_trans, z = 12)



# Save --------------------------------------------------------------------
writeRaster(clearwater_lake_elevation, file.path('inst', 'extdata', 'clearwater_lake_elevation.tif'))
