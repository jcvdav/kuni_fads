#######################
# clean_eez_shapefile #
#######################


##############################################
# This script reads in the entire world EEZ
# database and then keeps only the EEZs for
# the countries we care about.
##############################################

# Load packages
library(here)
library(sf)
library(tidyverse)

# Read in ISO3 codes of interest
iso <- read.csv(here("raw_data", "iso_codes.csv"),
                stringsAsFactors = F) %>% 
  rename('country' = 'name') %>% 
  pull(alpha_3)

# Load EEZ
eez <- st_read(here("raw_data", "eez")) %>% 
  filter(ISO_Ter1 %in% c(iso, "BMU")) %>%             # Filtering ISO 3 codes of interest
  group_by(ISO_Ter1) %>% 
  summarize(a = 0) %>% 
  select(ISO_Ter1) %>%                      # Keeping only column of interest
  rmapshaper::ms_simplify(keep_shapes = T)  # Simplify the EEZ boundary for faster computation

# Write out the geopackage file
st_write(obj = eez,
         dsn = here("data", "caribbean_eez.gpkg"),
         delete_layer = T)


