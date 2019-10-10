#####################
# crop_all_rasters  #
#####################

# Load packages
library(here)
library(raster)
library(tidyverse)

# Create an extent object for the Caribbean only
caribbean_extent <- extent(-90, -55, 5, 30)

# Create a function that reads, crops, and saves raster files
crop_caribbean <- function(layer, extent) {
  # Name of raster to read
  raster_in <- here::here("raw_data", layer, paste0(layer, ".asc"))
  
  # Name of raster to write
  raster_out <-
    here::here("data", "input", paste0(layer, ".tif"))
  
  # Make sure directories exist
  if (!dir.exists(here::here("data", "input"))) {
    print("creating missing directory: input")
    dir.create(here::here("data", "input"))
  }
  
  # Read, crop, write raster
  raster::raster(x = raster_in) %>%
    raster::crop(y = extent) %>%
    raster::writeRaster(filename = raster_out,
                        overwrite = TRUE)
}

# Depth
crop_caribbean(layer = "depth",
               extent = caribbean_extent)

# Surface current
crop_caribbean(layer = "surface_current",
               extent = caribbean_extent)

# Temperature
crop_caribbean(layer = "sstmean",
               extent = caribbean_extent)

# Mahi mahi distribution
raster(here("raw_data", "Coryphaena_hippurus.nc"),
       varname = "probability") %>% 
  crop(y = caribbean_extent) %>%
  writeRaster(filename = here("data", "input", "Coryphaena_hippurus.tif"),
              overwrite = T)

# Shipping lanes
## Read in a reference rastr to reproject to
reference <- raster(here("data", "input", "depth.tif")) 

# Read in shipping
shipping <- raster(here("raw_data", "shipping", "shipping.tif")) %>% 
  projectRaster(to = reference) %>%                   # reproject
  crop(y = caribbean_extent) %>%                     # crop to Caribbean
  focal(w = matrix(1, 5, 5), mean)                   # Apply a focal function to smooth

shipping <- shipping > 10 # Filter values > 10 as T

# Export the raster
writeRaster(x = shipping,
            filename = here("data", "input", "shipping_lanes.tif"),
            overwrite = T)

#######
#Distance to land

# Create a raster template for rasterizing the polys. 
# (set the desired grid resolution with res)
r <- raster(extent(reference), res = 0.083)

coast <- rnaturalearth::ne_countries(scale = "large", returnclass = "sf") %>% 
  sf::st_crop(y = extent(reference))

# Rasterize and set land pixels to NA
r2 <- rasterize(coast, reference, 1)
r3 <- mask(is.na(r2), r2, maskvalue=1, updatevalue=NA)
# Calculate distance to nearest non-NA pixel
d <- distance(r2)

# Optionally set non-land pixels to NA (otherwise values are "distance to non-land")
d2 <- ((d*r3) / 1e3) / 1.854

# Export the raster
writeRaster(x = d2,
            filename = here("data", "input", "landdistance.tif"),
            overwrite = T)


