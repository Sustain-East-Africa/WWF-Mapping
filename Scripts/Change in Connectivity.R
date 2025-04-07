library(terra)
raster1 <- rast("path to 2020 connectivity data") 
raster2 <- rast("path to 2024 connectivity data")


difference <- raster2 - raster1

plot(difference)
writeRaster(difference, "path/connectivity change.tif", overwrite=TRUE)
