library(data.table)
library(sf)
library(raster)

data(points)
data(lc)
data(water)
data(densitygrid)

DT <- fread(system.file("testdata", 'DT.csv', package = "wildcam"))