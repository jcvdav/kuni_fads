# visualize_cost_raster #

#### SET UP ####
# Load packages
library(startR)
library(here)
library(raster)
library(sf)
library(rnaturalearth)
library(tidyverse)

# Load data
## Cost rasters
cost <- raster(here("data", "cost.tif"))
croped_cost <- raster(here("data", "croped_cost.tif"))

## EEZ vector
eez <- st_read(here("data", "caribbean_eez.gpkg"))  %>% 
  arrange(ISO_Ter1) %>% 
  mutate(ID = group_indices(., ISO_Ter1))

## Convert EEZ to spatial
eez_sp <- eez %>% 
  as_Spatial()

## Coastline cropped to the extent of the cost raster
coast <- ne_countries(scale = "medium", returnclass = "sf") %>% 
  st_crop(y = extent(croped_cost))

# Convert the rasters to a data.frames
# for easy ggplot handling
cost_data <- as.data.frame(cost, xy = T)
croped_cost_data <- as.data.frame(croped_cost, xy = T)

# Visualize rasters
## For all data
all_costs <- ggplot() +
  geom_raster(data = cost_data,
              mapping = aes(x = x, y = y, fill = cost),
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

ggsave(plot = all_costs,
       filename = here("img", "all_costs.png"),
       width = 8, height = 5)

## For the croped data
croped_costs <- ggplot() +
  geom_raster(data = croped_cost_data,
              mapping = aes(x = x, y = y, fill = croped_cost),
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

ggsave(plot = croped_costs,
       filename = here("img", "croped_costs.png"),
       width = 8, height = 5)


## Country-level stats
# Extract all values by EEZ (for analyses)
all_values_by_country <- raster::extract(croped_cost,
                                         eez_sp,
                                         na.rm = T,
                                         df = T)

# Extract all values by EEZ and summarize with mean
# (for visualization)
summarized_values_by_country <- raster::extract(croped_cost,
                                                eez_sp,
                                                na.rm = T,
                                                fun = mean,
                                                sp = T) %>% 
  st_as_sf()

eez_with_data <- eez %>% 
  st_set_geometry(NULL) %>%
  left_join(all_values_by_country, by = "ID") %>% 
  drop_na(croped_cost)

cost_distribution <- ggplot(eez_with_data, aes(y = ISO_Ter1, x = croped_cost)) +
  ggridges::geom_density_ridges(quantile_lines = T,
                                quantiles = 2,
                                panel_scaling = T,
                                fill = "steelblue",
                                alpha = 0.5) +
  ggtheme_plot() +
  labs(x = "Cost ($USD)", y = "Country (ISO3 code)")

ggsave(plot = cost_distribution,
       filename = here("img", "cost_distribution.png"),
       height = 5,
       width = 3.5)

summarized_cost <- ggplot(summarized_values_by_country) +
  geom_sf(aes(fill = croped_cost)) +
  geom_sf(data = coast) +
  ggtheme_map() +
  scale_fill_gradientn(colours = colorRamps::matlab.like(100)) +
  guides(fill = guide_colorbar(title = "Costs ($USD)",
                               ticks.colour = "black",
                               frame.colour = "black")) +
  theme(legend.justification = c(1, 1),
        legend.position = c(1, 1),
        text = element_text(size = 15))

ggsave(plot = summarized_cost,
       filename = here("img", "summarized_cost.png"),
       height = 5,
       width = 8)
