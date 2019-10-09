## Cleaning + selecting nutrition data

# load packages:
library(here)
library(tidyverse)

# load files: (for references see metadata in nutrition_all.xlsx)
genus_intake <- read.csv(here("raw_data", "nutrition", "genus_intake.csv"), stringsAsFactors=F) %>%
  janitor::clean_names()
fao_intake <- read.csv(here("raw_data", "nutrition", "fao_intake.csv"), stringsAsFactors=F) %>%
  janitor::clean_names() %>%
  mutate(element_abr = tolower(substr(element, 1, 3)),
         element_abr = replace(element_abr, element_abr == "foo", "cal")) %>%
  mutate(item_abr = tolower(substr(item, 1, 3)),
         item_abr = replace(item_abr, item_abr == "mar", "mar_other"))
fao_fs <- read.csv(here("raw_data", "nutrition", "fao_fs_indicators.csv"), stringsAsFactors=F) %>%
  janitor::clean_names() %>%
  rename(country = area)
country_codes <- read.csv(here("raw_data", "country_codes.csv"), stringsAsFactors=F) %>%
  rename(country = alt_names)

# spread FAO data into wide format and merge sources
fao_fs_wide <- fao_fs %>%
  select(country, item_code, value) %>%
  spread(item_code, value)

fao_intake_wide <- fao_intake %>%
  select(country, element_abr, item_abr, value, year) %>%
  unite("element_item",c(element_abr, item_abr), sep= "_", remove=FALSE) %>%
  spread(element_item, value)

nutrition <- left_join(country_codes, fao_fs_wide, by = "country")

#################### keys for FAO item codes #################################
fao_fs_key <- fao_fs %>%
  group_by(item, item_code, unit, year) %>%
  summarize()
fao_intake_key <- fao_intake %>%
  group_by(element, element_code, element_abr, unit) %>%
  summarize()

