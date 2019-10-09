

library(tidyverse)
library(here)
library(janitor)
library(countrycode)

# file with country names and alpha_3 codes
iso <- read.csv(here("raw_data", "iso_codes.csv"), stringsAsFactors = F)

### nutrition + food security datasets
fao_fs <- fao_fs <- read.csv(here("raw_data", "nutrition", "fao_fs_indicators.csv"), stringsAsFactors = F) %>% 
  clean_names() %>%
  filter(area != "Caribbean") %>%
  select(area,item,value) %>%
  spread(item, value) %>%
  set_names("country","energy_adequacy","sev_insecurity","undernourishment") %>%
  mutate(alpha_3 = countrycode(country, 'country.name', 'iso3c')) %>%
  select(alpha_3, everything(), -country)

genus_intake <- read.csv(here("raw_data", "nutrition", "genus_intake.csv"), stringsAsFactors = F) %>% 
  clean_names() %>% # all from 2011
  mutate(calories_pf = calories_pelagicfish / calories,
         protein_pf = protein_pelagic_fish / protein) %>%
  select(country,calories_pf,protein_pf) %>%
  mutate(country= ifelse(country == "Netherlands Antilles", "Bonaire, Sint Eustatius and Saba", country)) %>% 
  mutate(alpha_3 = countrycode(country, 'country.name', 'iso3c')) %>%
  select(alpha_3, everything(), -country)

fao_intake <- read.csv(here("raw_data", "nutrition", "fao_intake.csv"), stringsAsFactors = F) %>% 
  clean_names() %>%
  filter(item == "Pelagic Fish", year == 2013, country != "Caribbean") %>% ### eventually calculate 3-year average?
  select(country,element,value) %>%
  spread(element,value) %>%
  set_names("country","fao_fat_pf","fao_cal_pf","fao_prot_pf") %>%
  mutate(alpha_3 = countrycode(country, 'country.name', 'iso3c')) %>%
  select(alpha_3, everything(), -country)
# currently not using FAO intake data because GENuS has total values for calculating percent intake

# selected nutrition variables:
nutrition <- fao_fs %>%
  select(alpha_3, energy_adequacy) %>% 
  full_join(genus_intake, by = "alpha_3")

### governance indicators
wgi <- read.csv(here("raw_data", "governance", "wgi_indicators.csv"), stringsAsFactors = F) %>%
  clean_names() %>%
  rename("value" = x2018_yr2018) %>%
  select(country_name, country_code, series_name, value) %>%
  spread(series_name, value) %>%
  set_names("country","alpha_3","corruption","gov_eff","pol_stab","reg_qual","rule_law","accountability") %>%
  mutate_at(vars(3:8),funs(as.numeric)) %>%
  rowwise() %>%
  mutate(wgi_mean = mean(c(corruption, gov_eff, pol_stab, reg_qual, rule_law, accountability), na.rm = T))
  select(alpha_3, everything(), -country)



