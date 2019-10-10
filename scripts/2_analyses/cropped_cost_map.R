# cropped_cost_map

# Load packages
library(startR)
library(here)
library(raster)
library(tidyverse)
library(rnaturalearth)
# library(rnaturalearthdata)

# Load rasters
## Biophysical data
depth <- raster(here("data", "input", "depth.tif"))
land_distance <- raster(here("data", "input", "landdistance.tif"))
sst <- raster(here("data", "input", "sstmean.tif"))
surface_current <- abs(raster(here("data", "input",  "surface_current.tif")))
shipping <- raster(here("data", "input", "shipping_lanes.tif"))

mahi_mahi <- raster(here("data", "input", "Coryphaena_hippurus.tif")) %>% 
  projectRaster(to = depth)

# Custom cutoffs that make no sense for now
max_depth <- -3000
min_depth <- -30
max_land_distance <- 50
min_sst <- 24
max_sst <- 41
max_surface_current <- 0.65 # A constant 0.65 current would hide a FAD with a 400L flotation device

# Create masks
depth_max_mask <- depth > max_depth
depth_min_mask <- depth < min_depth
land_distance_mask <- land_distance < max_land_distance
sst_min_mask <- sst > min_sst
sst_max_mask <- sst < max_sst
surface_current_mask <- surface_current < max_surface_current
mahi_mahi_mask <- mahi_mahi > 0.1
shipping_mask <- shipping == 0

stacked_masks <- stack(depth_max_mask,
                       depth_min_mask,
                       land_distance_mask,
                       sst_min_mask,
                       sst_max_mask,
                       surface_current_mask)

# Mask rasters
masked_depth <- mask(x = depth,
                     mask = depth_mask,
                     maskvalue = F)

masked_min_depth <- mask(x = depth,
                         mask = depth_min_mask,
                         maskvalue = F)

masked_land_distance <- mask(x = land_distance,
                             mask = land_distance_mask,
                             maskvalue = F)

masked_sst_min <- mask(x = sst,
                       mask = sst_min_mask,
                       maskvalue = F)

masked_sst_max <- mask(x = sst,
                       mask = sst_max_mask,
                       maskvalue = F)

masked_surface_current <- mask(x = surface_current,
                               mask = surface_current_mask,
                               maskvalue = F)

masked_shipping <- mask(x = shipping,
                        mask = shipping_mask,
                        maskvalue = F)

cost <- calc(depth, get_cost)

cost_df_full <- as.data.frame(cost, xy = T)


croped_cost <- cost %>% 
  mask(x = ., mask = mahi_mahi_mask, maskvalue = F) %>%
  mask(x = ., mask = depth_max_mask, maskvalue = F) %>% 
  mask(x = ., mask = depth_min_mask, maskvalue = F) %>%
  mask(x = ., mask = land_distance_mask, maskvalue = F) %>%
  mask(x = ., mask = sst_min_mask, maskvalue = F) %>%
  mask(x = ., mask = sst_max_mask, maskvalue = F) %>%
  mask(x = ., mask = surface_current_mask, maskvalue = F) %>%
  mask(x = ., mask = shipping_mask, maskvalue = F)

writeRaster(x = croped_cost,
            filename = here("data", "croped_cost.tif"),
            overwrite = T)

data <- croped_cost %>% 
  as.data.frame(xy = T)

coast <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") %>% 
  st_crop(y = extent(depth))

eez <- st_read(here("data", "caribbean_eez.gpkg"))

ggplot() +
  geom_raster(data = cost_df_full,
              mapping = aes(x = x, y = y, fill = layer)) +
  scale_fill_gradientn(colours = colorRamps::matlab.like(50), na.value = "transparent") +
  geom_sf(data = coast) +
  geom_sf(data = eez, fill = "transparent") +
  ggtheme_map() +
  guides(fill = guide_colorbar(title = "Costs ($USD)",
                               ticks.colour = "black",
                               frame.colour = "black")) +
  theme(legend.justification = c(1, 1),
        legend.position = c(1, 1))


ggplot() +
  geom_raster(data = data,
              mapping = aes(x = x, y = y, fill = layer),
              interpolate = T) +
  scale_fill_gradientn(colours = colorRamps::matlab.like(50),
                       na.value = "transparent",
                       ) +
  geom_sf(data = coast) +
  geom_sf(data = eez, fill = "transparent") +
  ggtheme_map() +
  guides(fill = guide_colorbar(title = "Costs ($USD)",
                               ticks.colour = "black",
                               frame.colour = "black")) +
  theme(legend.justification = c(1, 1),
        legend.position = c(1, 1),
        text = element_text(size = 15))















