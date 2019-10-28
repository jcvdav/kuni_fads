### Map of estimated FAD numbers
library(here)
library(ggsflabel)
library(rnaturalearth)
library(sf)
library(tidyverse)

# Load fad data
fad_data <- read.csv(here("raw_data", "fad_data", "fads_current.csv"))

## EEZ vector
eez <- st_read(here("data", "caribbean_eez.gpkg"))  %>% 
  arrange(ISO_Ter1) %>% 
  mutate(ID = group_indices(., ISO_Ter1),
         area = st_area(.))

## Coastline cropped to the extent of the cost raster
coast <- ne_countries(scale = "medium", returnclass = "sf") %>% 
  st_crop(y = st_bbox(eez))

# Create points form centroids
points <- coast %>% 
  left_join(fad_data, by = c("adm0_a3" = "alpha_3")) %>% 
  drop_na(n_fads) %>% 
  st_point_on_surface()

# Create plot
map <- ggplot() +
  geom_sf(data = coast, fill = "black", color = "black") +
  geom_sf(data = points,
          color = "black",
          fill = "red",
          shape = 21) +
  geom_sf_text_repel(data = points, aes(label = n_fads),
                     nudge_x = 2,
                     nudge_y = 2,
                     min.segment.length = 0,
                     seed = 1) +
  ggtheme_map()

# Save plot
ggsave(plot = map,
       filename = here("img", "fad_map.pdf"),
       width = 5,
       height = 4)
