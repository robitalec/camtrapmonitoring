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



# Crop --------------------------------------------------------------------
clearwater_lake_land_cover <- crop(ca_lc, clearwater_lake_extent)



# Save --------------------------------------------------------------------
usethis::use_data(clearwater_lake_land_cover, overwrite = TRUE)
