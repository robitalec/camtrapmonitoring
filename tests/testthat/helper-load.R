library(data.table)
library(sf)
library(raster)

data(points)
data(lc)
data(water)
data(densitygrid)

DT <- data.table(ID = points$ID, st_coordinates(points))