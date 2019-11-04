### Map of estimated FAD numbers
library(here)
library(ggsflabel)
library(rnaturalearth)
library(sf)
library(tidyverse)

# Load fad data
fad_data_now <- read.csv(here("raw_data", "fad_data", "fads_current.csv"))

fad_data_before <- read.csv(here("raw_data", "fad_data", "fads_timeline.csv")) %>% 
  clean_names() %>% 
  rename(name = i_name) %>% 
  filter(year == 2001)

## EEZ vector
eez <- st_read(here("data", "caribbean_eez.gpkg"))  %>% 
  arrange(ISO_Ter1) %>% 
  mutate(ID = group_indices(., ISO_Ter1),
         area = st_area(.))

# Create points form centroids
points_before <- eez %>% 
  left_join(fad_data_before, by = c("ISO_Ter1" = "alpha_3")) %>% 
  drop_na(n_fads) %>% 
  filter(n_fads > 0) %>% 
  st_point_on_surface()

points_now <- eez %>% 
  left_join(fad_data_now, by = c("ISO_Ter1" = "alpha_3")) %>% 
  drop_na(n_fads) %>% 
  filter(n_fads > 0) %>% 
  st_point_on_surface()

# Create plot

## MAP BEFORE
map_before <- ggplot() +
  geom_sf(data = coast, fill = "black", color = "black") +
  stat_sf_coordinates(data = points_before, aes(size = n_fads),
                      geom = "point",
                      color = "black",
                      fill = "red",
                      shape = 21) +
  geom_sf_text_repel(data = points_before, aes(label = n_fads),
                     nudge_x = 0.5,
                     nudge_y = 1,
                     min.segment.length = 0,
                     seed = 1) +
  ggtheme_map() +
  scale_size_continuous(trans = "log10", range = c(1, 4), breaks = c(10, 100, 1000)) +
  guides(size = guide_legend(title = "Number of FADs")) +
  ggtitle("Estimated MFAD numbers (2001)") +
  theme(legend.justification = c(1, 1),
        legend.position = c(1, 1))

# Save plot
ggsave(plot = map_before,
       filename = here("img", "fad_map_before.pdf"),
       width = 5,
       height = 4)

ggsave(plot = map_before,
       filename = here("img", "fad_map_before.png"),
       width = 5,
       height = 4)

## MAP TODAY
map <- ggplot() +
  geom_sf(data = coast, fill = "black", color = "black") +
  stat_sf_coordinates(data = points_now, aes(size = n_fads),
                      geom = "point",
                      color = "black",
                      fill = "red",
                      shape = 21) +
  geom_sf_text_repel(data = points_now, aes(label = n_fads),
                     nudge_x = 1,
                     nudge_y = 1,
                     min.segment.length = 0,
                     seed = 1) +
  ggtheme_map() +
  scale_size_continuous(trans = "log10", range = c(1, 6), breaks = c(10, 100, 1000)) +
  guides(size = guide_legend(title = "Number of FADs")) +
  ggtitle("Estimated MFAD numbers (present)") +
  theme(legend.justification = c(1, 1),
        legend.position = c(1, 1))

# Save plot
ggsave(plot = map,
       filename = here("img", "fad_map_now.pdf"),
       width = 5,
       height = 4)

# Save plot
ggsave(plot = map,
       filename = here("img", "fad_map_now.png"),
       width = 5,
       height = 4)
