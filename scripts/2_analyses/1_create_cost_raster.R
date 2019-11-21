# cropped_cost_map

#### set up  ####
# Load packages
library(startR)
library(here)
library(raster)
library(sf)
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
surface_current <- abs(raster(here("data", "input",  "surface_current.tif")))
names(surface_current) <- "current"

shipping <- raster(here("data", "input", "shipping_lanes.tif"))
mahi_mahi <- raster(here("data", "input", "Coryphaena_hippurus.tif")) %>% 
  projectRaster(to = depth)
wahoo <- raster(here("data", "input", "Acanthocybium_solandri.tif")) %>% 
  projectRaster(to = depth)
skipjack <- raster(here("data", "input", "Katsuwonus_pelamis.tif")) %>% 
  projectRaster(to = depth)
yellowfin <- raster(here("data", "input", "Thunnus_albacares.tif")) %>% 
  projectRaster(to = depth)

# Cutoffs
max_depth <- -3000          # max depth is 3000 meters
min_depth <- -100            # min depth is 30 meters
max_land_distance <- 50     # Max distance from land is 50 nautical miles
max_surface_current <- 0.65 # A constant 0.65 current would hide a FAD with a 400L flotation device
spp_threshold <- 0.25       # Probability threshold for species presence

#### Create masks ####
# The following lines create rasters with values
# 0 / 1 to indicate if they meet a criteria or not
## Depth
depth_max_mask <- depth > max_depth
depth_min_mask <- depth < min_depth
depth_mask <- depth_max_mask * depth_min_mask
## Land
land_distance_mask <- land_distance < max_land_distance
## Surface curent
surface_current_mask <- surface_current < max_surface_current
## Mahi mahi
mahi_mask <- mahi_mahi > spp_threshold
skj_mask <- skipjack > spp_threshold
wahoo_mask <- wahoo > spp_threshold
yellowfin_mask <- yellowfin > spp_threshold

spp_mask <- (mahi_mask + skj_mask + wahoo_mask + yellowfin_mask) > 0

## Shipping
shipping_mask <- shipping == 0

### ALL MASKS PUT TOGETHER
all_masks <- spp_mask * depth_mask * land_distance_mask * surface_current_mask * shipping_mask

#### Calculate costs ####
# Apply the get_cost function on the depth raster
deployment_cost <- overlay(depth, surface_current, fun = deployment_cost) * all_masks
deployment_cost[deployment_cost == 0] <- NA

travel_cost <- calc(land_distance, travel_cost) * all_masks
travel_cost[travel_cost == 0] <- NA

writeRaster(x = deployment_cost,
            filename = here("data", "deployment_cost.tif"),
            overwrite = T)

writeRaster(x = travel_cost,
            filename = here("data", "travel_cost.tif"),
            overwrite = T)

# Add costs

cost <- deployment_cost + travel_cost

# Save a complete cost raster
writeRaster(x = cost,
            filename = here("data", "cropped_cost.tif"),
            overwrite = T)

## END OF SCRIPT














