# cropped_cost_map

#### set up  ####
# Load packages
library(startR)
library(here)
library(raster)
library(magrittr)
library(tidyverse)
library(rnaturalearth)

# load functions that calculate cost
source(here("scripts", "2_analyses", "cost_functions.R"))

#### Data load ####
# Load rasters
## Biophysical data
depth <- raster(here("data", "input", "depth.tif"))
land_distance <- raster(here("data", "input", "landdistance.tif"))
sst <- raster(here("data", "input", "sstmean.tif"))
surface_current <- abs(raster(here("data", "input",  "surface_current.tif")))
shipping <- raster(here("data", "input", "shipping_lanes.tif"))
mahi_mahi <- raster(here("data", "input", "Coryphaena_hippurus.tif")) %>% 
  projectRaster(to = depth)

# Cutoffs
max_depth <- -3000          # max depth is 3000 meters
min_depth <- -30            # min depth is 30 meters
max_land_distance <- 50     # Max distance from land is 50 nautical miles
min_sst <- 24               # Minimum SST is 24
max_sst <- 41               # Maximum SST is 41 (mahi upper limit on Aquamaps)
max_surface_current <- 0.65 # A constant 0.65 current would hide a FAD with a 400L flotation device
mahi_mahi_threshold <- 0.5  # Probability threshold for mahi mahi presence

#### Create masks ####
# The following lines create rasters with values
# 0 / 1 to indicate if they meet a criteria or not
## Depth
depth_max_mask <- depth > max_depth
depth_min_mask <- depth < min_depth
depth_mask <- depth_max_mask * depth_min_mask
## Land
land_distance_mask <- land_distance < max_land_distance
## SST
sst_min_mask <- sst > min_sst
sst_max_mask <- sst < max_sst
sst_mask <- sst_min_mask * sst_max_mask
## Surface curent
surface_current_mask <- surface_current < max_surface_current
## Mahi mahi
mahi_mahi_mask <- mahi_mahi > 0.5
## Shipping
shipping_mask <- shipping == 0

# Stack all masks to have a 
# raster stack for visualization
stacked_masks <- stack(depth_mask,
                       land_distance_mask,
                       sst_mask,
                       surface_current_mask,
                       mahi_mahi_mask,
                       shipping_mask)

names(stacked_masks) <- c("depth", "land_distance", "sst", "surface_current", "Mahi_mahi", "shipping")

plot(stacked_masks)

#### Calculate costs ####
# Apply the get_cost function on the depth raster
cost <- calc(depth, get_cost)

# Save a complete cost raster
writeRaster(x = cost,
            filename = here("data", "cost.tif"),
            overwrite = T)

# Load coastline for step-by-step visualization
coast <- ne_countries(scale = "medium", returnclass = "sf") %>% 
  st_crop(y = extent(depth))

# Create a function that takes a snapshot of the
# raster after each crop and exports a png
snapshot <- function(r, step) {
  # Set filename
  step <- paste0(step, ".png")
  
  # Create the plot
  p <- as.data.frame(r, xy = T) %>% 
    ggplot() +
    geom_raster(aes(x = x, y = y, fill = layer)) +
    geom_sf(data = coast) +
    scale_fill_gradientn(colours = colorRamps::matlab.like(50),
                         na.value = "transparent") +
    startR::ggtheme_map() +
    guides(fill = guide_colorbar(title = "Costs ($USD)",
                                 ticks.colour = "black",
                                 frame.colour = "black"))
  # Export the plot
  ggsave(plot = p,
         filename = here("img", "raster_crops", step))
}

# step-by-step cropping
croped_cost <- cost %T>% 
  snapshot(step = 1) %>% 
  mask(x = ., mask = mahi_mahi_mask, maskvalue = F) %T>%
  snapshot(step = 2) %>% 
  mask(x = ., mask = depth_mask, maskvalue = F) %T>%
  snapshot(step = 3) %>% 
  mask(x = ., mask = land_distance_mask, maskvalue = F) %T>%
  snapshot(step = 4) %>% 
  mask(x = ., mask = sst_mask, maskvalue = F) %T>%
  snapshot(step = 5) %>% 
  mask(x = ., mask = surface_current_mask, maskvalue = F) %T>%
  snapshot(step = 6) %>% 
  mask(x = ., mask = shipping_mask, maskvalue = F)

# Final snapshot
snapshot(croped_cost, step = 7)

# Export the croped raster
writeRaster(x = croped_cost,
            filename = here("data", "croped_cost.tif"),
            overwrite = T)


## END OF SCRIPT














