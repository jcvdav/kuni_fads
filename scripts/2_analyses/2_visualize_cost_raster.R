# visualize_cost_raster #

#### SET UP ####
# Load packages
library(startR)
library(here)
library(raster)
library(sf)
library(rnaturalearth)
library(cowplot)
library(tidyverse)

# Load data
## Total costs
croped_cost <- raster(here("data", "croped_cost.tif"))

## Deployment costs
deployment_cost <- raster(here("data", "deployment_cost.tif"))

## Travel costs
travel_cost <- raster(here("data", "travel_cost.tif"))

## EEZ vector
eez <- st_read(here("data", "caribbean_eez.gpkg"))  %>% 
  arrange(ISO_Ter1) %>% 
  mutate(ID = group_indices(., ISO_Ter1),
         area = st_area(.))

## Convert EEZ to spatial
eez_sp <- eez %>% 
  as_Spatial()

## Coastline cropped to the extent of the cost raster
coast <- ne_countries(scale = "medium", returnclass = "sf") %>% 
  st_crop(y = extent(croped_cost))

# Convert the rasters to a data.frames
# for easy ggplot handling
croped_cost_data <- as.data.frame(croped_cost, xy = T)
deployment_cost_data <- as.data.frame(deployment_cost, xy = T)
travel_cost_data <- as.data.frame(travel_cost, xy = T)

# Visualize rasters
## For the deployment costs
(deployment_costs <- ggplot() +
    geom_raster(data = deployment_cost_data,
                mapping = aes(x = x, y = y, fill = deployment_cost),
                interpolate = T) +
    scale_fill_gradientn(colours = colorRamps::matlab.like(50),
                         na.value = "transparent",
    ) +
    geom_sf(data = coast) +
    geom_sf(data = eez, fill = "transparent") +
    ggtheme_map() +
    theme(legend.position = "bottom") +
    guides(fill = guide_colorbar(title = "Costs ($USD)",
                                 ticks.colour = "black",
                                 frame.colour = "black")) +
    scale_y_continuous(expand = c(0, 0)) +
    scale_x_continuous(expand = c(0, 0)))

## For the travel costs
(travel_costs <- ggplot() +
  geom_raster(data = travel_cost_data,
              mapping = aes(x = x, y = y, fill = travel_cost),
              interpolate = T) +
  scale_fill_gradientn(colours = colorRamps::matlab.like(50),
                       na.value = "transparent",
  ) +
  geom_sf(data = coast) +
  geom_sf(data = eez, fill = "transparent") +
  ggtheme_map() +
  theme(legend.position = "bottom") +
  guides(fill = guide_colorbar(title = "Costs ($USD)",
                               ticks.colour = "black",
                               frame.colour = "black")) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_continuous(expand = c(0, 0)))



# Extract all values by EEZ and summarize with mean
# (for visualization)
summarized_values_by_country <- raster::extract(croped_cost,
                                                eez_sp,
                                                na.rm = T,
                                                fun = median,
                                                sp = T) %>% 
  st_as_sf()

# Plot the costs at the EEZ-level
(summarized_cost <- ggplot(summarized_values_by_country) +
    geom_sf(data = eez, fill = "transparent") +
    geom_sf(aes(fill = croped_cost)) +
    geom_sf(data = coast) +
    ggtheme_map() +
    scale_fill_gradientn(colours = colorRamps::matlab.like(100)) +
    guides(fill = guide_colorbar(title = "Costs ($USD)",
                                 ticks.colour = "black",
                                 frame.colour = "black")) +
    theme(legend.justification = c(0, 0),
          legend.position = c(0, 0)) +
    scale_x_continuous(expand = c(0 ,0)) +
    scale_y_continuous(expand = c(0 ,0)))


subplots <- plot_grid(deployment_costs,
                      travel_costs,
                      ncol = 2,
                      labels = c("B", "C"))

plot <- plot_grid(summarized_cost,
                  subplots,
                  ncol = 1,
                  rel_heights = c(2, 1),
                  align = "v",
                  labels = c("A", NA))

ggsave(plot = plot,
       filename = here("img", "cost_map.png"),
       height = 7.5,
       width = 5)


## Country-level stats
# Extract all values by EEZ (for analyses)
all_values_by_country <- raster::extract(travel_cost,
                                         eez_sp,
                                         na.rm = T,
                                         df = T)

# Add ISO3 codes
eez_with_data <- eez %>% 
  st_set_geometry(NULL) %>%
  left_join(all_values_by_country, by = "ID") %>% 
  drop_na()

# Plot the distribution of costs
(cost_distribution <- ggplot(eez_with_data, aes(y = reorder(ISO_Ter1, travel_cost, FUN = median), x = travel_cost)) +
    ggridges::geom_density_ridges(quantile_lines = T,
                                  quantiles = 2,
                                  panel_scaling = T,
                                  fill = "steelblue",
                                  alpha = 0.5) +
    # geom_boxplot() +
    ggtheme_plot() +
    labs(x = "Cost ($USD)", y = "Country (ISO3 code)"))

# Save the plot with the distribution of costs
ggsave(plot = cost_distribution,
       filename = here("img", "cost_distribution.png"),
       height = 5,
       width = 3.5)


# Calculate various country-level stats
country_level_summary_statistics <- eez_with_data %>% 
  group_by(ISO_Ter1) %>% 
  summarize(mean = mean(croped_cost),
            median = median(croped_cost),
            sd = sd(croped_cost),
            min = min(croped_cost),
            max = max(croped_cost),
            pct10 = quantile(croped_cost, 0.1),
            pct25 = quantile(croped_cost, 0.25),
            pct75 = quantile(croped_cost, 0.75),
            pct09 = quantile(croped_cost, 0.9)) %>% 
  rename(ISO3 = ISO_Ter1)

# Export the csv of coutnry-level stats
write.csv(x = country_level_summary_statistics,
          file = here("data", "country_level_cost_summary_statistics.csv"),
          row.names = F)

# END OF SCRIPT

