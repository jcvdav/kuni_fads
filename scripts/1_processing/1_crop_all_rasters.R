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

# Distance to land
crop_caribbean(layer = "landdistance",
               extent = caribbean_extent)

# Distance to port
crop_caribbean(layer = "port_distance",
               extent = caribbean_extent)

# Surface current
crop_caribbean(layer = "surface_current",
               extent = caribbean_extent)

# Wind speed
crop_caribbean(layer = "windspeed",
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






