# cropped_cost_map

# Load rasters
depth <- raster(here("data", "spatial", "raster", "depth.tif"))
land_distance <- raster(here("data", "spatial", "raster", "landdistance.tif"))
port_distance <- raster(here("data", "spatial", "raster", "port_distance.tif"))
sst <- raster(here("data", "spatial", "raster", "sstmean.tif"))
surface_current <- abs(raster(here("data", "spatial", "raster", "surface_current.tif")))
wind_speed <- raster(here("data", "spatial", "raster", "windspeed.tif"))
mahi_mahi <- raster(here("data", "spatial", "raster", "Coryphaena_hippurus.tif"))


files <- list.files(path = here("data", "spatial", "raster"), pattern = "*.tif")
paths <- here("data", "spatial", "raster", files)

r <- stack(paths)


# Custom cutoffs that make no sense for now
max_depth <- -3000
max_land_distance <- 2
max_port_distance <- 3
min_sst <- 24
max_sst <- 31
max_surface_current <- 0.1
max_wind_speed <- 7

# Create masks
depth_mask <- depth > max_depth
land_distance_mask <- land_distance < max_land_distance
port_distance_mask <- port_distance < max_port_distance
sst_min_mask <- sst > min_sst
sst_max_mask <- sst < max_sst
surface_current_mask <- surface_current < max_surface_current
wind_speed_mask <- wind_speed < max_wind_speed
mahi_mahi_mask <- mahi_mahi > 0.1

stacked_masks <- stack(depth_mask,
                       land_distance_mask,
                       port_distance_mask,
                       sst_min_mask,
                       sst_max_mask,
                       surface_current_mask,
                       wind_speed_mask)

# Mask rasters
masked_depth <- mask(x = depth,
                     mask = depth_mask,
                     maskvalue = F)

masked_land_distance <- mask(x = land_distance,
                             mask = land_distance_mask,
                             maskvalue = F)

masked_port_distance <- mask(x = port_distance,
                             mask = port_distance_mask,
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

masked_wind_speed <- mask(x = wind_speed,
                          mask = wind_speed_mask,
                          maskvalue = F)

price_per_meter <- 1.2

current_factor <- 1.7 * (surface_current / max_surface_current)

cost <- -1 * depth * 7 * price_per_meter * current_factor

cost_df_full <- as.data.frame(cost, xy = T)


data <- cost %>% 
  # mask(x = ., mask = mahi_mahi_mask, maskvalue = F) %>% 
  mask(x = ., mask = depth_mask, maskvalue = F) %>% 
  mask(x = ., mask = land_distance_mask, maskvalue = F) %>% 
  mask(x = ., mask = port_distance_mask, maskvalue = F) %>%
  mask(x = ., mask = sst_min_mask, maskvalue = F) %>% 
  mask(x = ., mask = sst_max_mask, maskvalue = F) %>% 
  mask(x = ., mask = surface_current_mask, maskvalue = F) %>% 
  # mask(x = ., mask = wind_speed_mask, maskvalue = F) %>%
  as.data.frame(xy = T)

coast <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") %>% 
  sf::st_crop(y = extent(depth))

ggplot() +
  geom_raster(data = cost_df_full,
              mapping = aes(x = x, y = y, fill = layer)) +
  scale_fill_gradientn(colours = colorRamps::matlab.like(50), na.value = "transparent") +
  geom_sf(data = coast) +
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
  scale_fill_gradientn(colours = colorRamps::matlab.like(50), na.value = "transparent") +
  geom_sf(data = coast) +
  ggtheme_map() +
  guides(fill = guide_colorbar(title = "Costs ($USD)",
                               ticks.colour = "black",
                               frame.colour = "black")) +
  theme(legend.justification = c(1, 1),
        legend.position = c(1, 1))















