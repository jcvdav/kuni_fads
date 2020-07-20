### Map of estimated FAD numbers
library(janitor)
library(startR)
library(here)
library(ggsflabel)
library(rnaturalearth)
library(sf)
library(tidyverse)

# Load fad data
fad_data_now <- read.csv(here("raw_data", "fad_data", "fads_current.csv"),
                         stringsAsFactors = F) %>% 
  select(alpha_3, n_fads)

fad_data_before <- read.csv(here("raw_data", "fad_data", "fads_timeline.csv"),
                            stringsAsFactors = F) %>% 
  clean_names() %>% 
  filter(year == 2001)%>% 
  select(alpha_3, n_fads)

# EEZ vector
eez <- st_read(here("data", "caribbean_eez.gpkg"), stringsAsFactors = F)

coast <- ne_countries(returnclass = "sf", scale = "large") %>% 
  st_crop(st_bbox(eez)) %>% 
  mutate(adm0_a3 = case_when(adm0_a3 == "NLD" ~ "BES",
                             adm0_a3 == "FRA" ~ "GLP",
                             T ~ adm0_a3))

# Create points form centroids
mtq_centroid <- tibble(adm0_a3 = c("MTQ", "GLP"),
                       geometry = c(st_sfc(st_point(c(-61.024694, 14.690912))),
                                    st_sfc(st_point(c(-61.66, 16.18))))) %>% 
  st_sf(crs = 4326)

points_before <- coast %>% 
  select(adm0_a3) %>% 
  filter(!adm0_a3 == "GLP") %>% 
  rbind(mtq_centroid) %>% 
  left_join(fad_data_before, by = c("adm0_a3" = "alpha_3")) %>% 
  filter(n_fads > 0) %>% 
  st_point_on_surface()

points_now <- coast %>% 
  select(adm0_a3) %>% 
  filter(!adm0_a3 == "GLP") %>% 
  rbind(mtq_centroid) %>% 
  left_join(fad_data_now, by = c("adm0_a3" = "alpha_3")) %>% 
  filter(n_fads > 0) %>% 
  st_point_on_surface()

# Create plot

ggtheme_map <- function(base_size = 9) {
  theme(
    text = element_text(#family = "Helvetica",
      color = "gray30",
      size = base_size),
    plot.title = element_text(
      size = rel(1.25),
      hjust = 0,
      face = "bold"),
    panel.background = element_blank(),
    legend.position = "right",
    strip.background = element_rect(
      fill = "transparent"),
    panel.border = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(
      color = "transparent"),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    legend.key = element_rect(
      colour = NA,
      fill = NA),
    axis.line = element_blank()
  )
}

## MAP BEFORE
map_before <- ggplot() +
  geom_sf(data = coast, fill = "lightgray", color = "black") +
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
       width = 7,
       height = 6)

ggsave(plot = map_before,
       filename = here("img", "fad_map_before.png"),
       width = 7,
       height = 6)

## MAP TODAY
map <- ggplot() +
  geom_sf(data = coast, fill = "lightgray", color = "black") +
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
  scale_size_continuous(range = c(1, 6), breaks = c(1, 10, 100, 1000)) +
  guides(size = guide_legend(title = "Number of MFADs")) +
  ggtitle("") +
  theme(legend.justification = c(1, 1),
        legend.position = c(1, 1))

# Save plot
ggsave(plot = map,
       filename = here("img", "fad_map_now.pdf"),
       width = 7,
       height = 6)

# Save plot
ggsave(plot = map,
       filename = here("img", "fad_map_now.png"),
       width = 7,
       height = 6)
