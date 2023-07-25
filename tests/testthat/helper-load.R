library(sf)
library(raster)
library(data.table)

data(points)
data(lc)
data(water)
data(densitygrid)

DT <- fread(system.file("testdata", 'DT.csv', package = "camtrapmonitoring"))
