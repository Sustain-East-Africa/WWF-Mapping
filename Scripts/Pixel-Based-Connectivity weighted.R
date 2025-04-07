# Load required packages
library(terra)
library(sf)

# ---- 1. Read and Prepare the Landscape ----
landscape_sf <- st_read("path to AOI.shp")
landscape <- vect(landscape_sf)
landscape_proj <- project(landscape, "EPSG:32737")  # UTM Zone 37S

# Create a raster template with a 100m resolution
r_template <- rast(ext(landscape_proj), resolution = 100, crs = crs(landscape_proj))

# ---- 2. Read, Validate, and Prepare the Fence Data ----
fences_sf <- st_read("path to Fences.shp")
fences_sf <- st_make_valid(fences_sf)

# Reproject if needed
if (st_crs(fences_sf) != st_crs(landscape_proj)) {
  fences_sf <- st_transform(fences_sf, st_crs(landscape_proj))
}

fences_sf <- st_cast(fences_sf, "LINESTRING")
fences <- vect(fences_sf)

# Assign different weights based on fence Type
fences$value <- ifelse(fences$Type == 1, 100,  # Brush Fence (high connectivity)
                       ifelse(fences$Type == 2, 25,   # Wire Fence (low connectivity)
                              50))                    # Other (medium connectivity)

# ---- 3. Rasterize the Fence Data ----
fence_raster <- rasterize(fences, r_template, field = "value", background = 0)

# ---- 4. Create a Circular Kernel for a 5 km Radius ----
radius_m <- 5000
cell_size <- res(fence_raster)[1]
radius_cells <- radius_m / cell_size  
window_size <- (2 * radius_cells) + 1

offsets <- expand.grid(x = -radius_cells:radius_cells, y = -radius_cells:radius_cells)
offsets$dist <- sqrt(offsets$x^2 + offsets$y^2)

kernel <- matrix(ifelse(offsets$dist <= radius_cells, 1, NA), 
                 nrow = window_size, byrow = TRUE)

# ---- 5. Perform the Focal Operation ----
fence_buffer_avg <- focal(fence_raster, w = kernel, fun = mean, na.rm = TRUE)

# ---- 6. Save and Plot the Results ----
writeRaster(fence_buffer_avg, filename = "path/Connectivity.tif", overwrite = TRUE)
plot(fence_buffer_avg, main = "5 km Neighborhood Weighted Mean (terra)")
