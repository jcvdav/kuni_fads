### Cleaning and merging nutrition, WGI, and trade data

library(tidyverse)
library(here)
library(janitor)
library(countrycode)

# file with country names and alpha_3 codes
iso <- read.csv(here("raw_data", "iso_codes.csv"), stringsAsFactors = F)

############################## NUTRITION + FOOD SECURITY ####################################
fao_intake <- read.csv(here("raw_data", "nutrition", "fao_intake.csv"), stringsAsFactors = F) %>% 
  clean_names() %>%
  filter(item == "Pelagic Fish", year == 2013, country != "Caribbean") %>% ### eventually calculate 3-year average?
  select(country,element,value) %>%
  spread(element,value) %>%
  set_names("country","fao_fat_pf","fao_cal_pf","fao_prot_pf") %>%
  mutate(alpha_3 = countrycode(country, 'country.name', 'iso3c')) %>%
  select(alpha_3, everything(), -country)

fao_fs <- fao_fs <- read.csv(here("raw_data", "nutrition", "fao_fs_indicators.csv"), stringsAsFactors = F) %>% 
  clean_names() %>%
  filter(area != "Caribbean") %>%
  select(area,item,value) %>%
  spread(item, value) %>%
  set_names("country","energy_adequacy","sev_insecurity","undernourishment") %>%
  mutate(alpha_3 = countrycode(country, 'country.name', 'iso3c')) %>%
  select(alpha_3, everything(), -country)
### energy_adequacy is the only variable with reasonable coverage

genus_intake <- read.csv(here("raw_data", "nutrition", "genus_intake.csv"), stringsAsFactors = F) %>%
  clean_names() %>% # all from 2011
  select(country,calories_pelagicfish,fat_pelagicfish,protein_pelagic_fish) %>%
  set_names("country","gen_cal_pf","gen_fat_pf","gen_prot_pf") %>%
  mutate(alpha_3 = countrycode(country, 'country.name', 'iso3c')) %>%
  select(alpha_3, everything(), -country)
### won't use GENuS data, because has same country coverage as FAO for protein, fat, and calories and FAO data is more recent (GENuS = 2011)

# selected nutrition variables:
nutrition <- fao_fs %>%
  select(alpha_3, energy_adequacy) %>% 
  full_join(fao_intake, by = "alpha_3")

######################## FAO TRADE DATA ################################

# Defining FAD fisheries

fad_fished <- c("Albacore (=Longfin tuna), fresh or chilled", "Albacore (=Longfin tuna), frozen, nei", "Atlantic (Thunnus thynnus) and Pacific (Thunnus orientalis) bluefin tuna, frozen", "Euthynnus other than skipjack prep. or pres. not minced, nei", "Skipjack tuna, frozen", "Southern bluefin tuna (Thunnus maccoyii), live", "Tuna loins and fillets, frozen", "Tuna loins, prepared or preserved", "Tunas nei, frozen", "Tunas prepared or preserved, not minced, in oil", "Tunas prepared or preserved, not minced, nei", "Tunas, fresh or chilled, nei", "Yellowfin tuna, fresh or chilled", "Yellowfin tuna, frozen, nei", "Atlantic (Thunnus thynnus), Pacific (T.orientalis) bluefin tuna, live", "Atlantic (Thunnus thynnus)and Pacific (Thunnus orientalis) bluefin tuna, fresh or chilled", "Bigeye tuna, fresh or chilled", "Skipjack tuna, fresh or chilled", "Bigeye tuna, frozen, nei", "Dolphinfishes, fresh or chilled", "Dolphinfishes, frozen", "Miscellaneous pelagic fish fillets, frozen, nei", "Miscellaneous pelagic fish, fillets, fresh or chilled, nei", "Miscellaneous pelagic fish, nei, fresh or chilled", "Miscellaneous pelagic fish, nei, frozen", "Skipjack prepared or preserved, not minced, nei", "Southern bluefin tuna (Thunnus maccoyii), fresh or chilled", "Southern bluefin tuna (Thunnus maccoyii), frozen", "Tunas prepared or preserved, not minced, in airtight containers", "Tunas, bonitos, billfishes, fresh or chilled, nei", "Tunas, bonitos, billfishes, frozen, nei", "Tunas, flakes and grated, prepared or preserved", "Tuna loins and fillets, fresh or chilled", "Tuna meat, whether or not minced, frozen", "Tunas, bonitos, billfishes, meat, whether or not minced, frozen, nei", "Tunas, bonitos, billfishes, nei, minced, prepared or preserved", "Bonito (Sarda spp.), not minced, prepared or preserved, nei", "Miscellaneous pelagic fish nei, minced, prepared or preserved", "Skipjack, prepared or preserved, whole or in pieces, not minced, in oil", "Tunas nei, minced, prepared or preserved", "Yellowfin tuna, heads-off, etc., frozen", "Albacore (=Longfin tuna), gilled, gutted, frozen", "Albacore (=Longfin tuna), heads-off, etc., frozen", "Euthynnus excl. skipjack or stripe-bellied bonitos, fresh or chilled", "Euthynnus excl. skipjack or stripe-bellied bonitos, frozen", "Tunas, gilled, gutted, frozen, nei", "Yellowfin tuna, gilled, gutted, frozen", "Tunas, heads-off, etc., frozen, nei", "Miscellaneous pelagic fish nei, dried, whether or not salted", "Miscellaneous pelagic fish nei, fillets, dried, salted or in brine", "Marlins, fresh or chilled", "Marlins, frozen", "Tunas, bonitos, billfishes fillets, fresh or chilled, nei", "Tunas, bonitos, billfishes etc, fillets, frozen, nei")

# All tidy 
trade_fish <- read.csv(here("raw_data/trade/AllMarineFish.tidy.csv"), header = T, stringsAsFactors = F, na.strings = c("...", "-")) %>% 
  clean_names() %>%
  select(-(c(x_1, x, x_f))) %>% 
  filter(country != 'Totals') %>% 
  mutate(flow = ifelse(flow == "Reexports", "Exports", flow)) %>% # grouping reexports with exports (see note below)
  mutate(country= ifelse(country == "Netherlands Antilles", "Bonaire, Sint Eustatius and Saba", ifelse(country == "CuraÃ§ao", 'Curaçao', country))) %>% 
  mutate(alpha_3 = countrycode(country, "country.name", "iso3c")) %>%
  mutate(fad_fished  = ifelse(commodity %in% fad_fished, 1, 0)) %>%
  filter(fad_fished == 1) %>%
  group_by(country, flow, year) %>%
  summarize(quantity_fad = sum(quantity, na.rm = T))


  group_by(alpha_3, commodity, flow) %>%
  summarise(quantity = mean(quantity, na.rm = T)) %>%
  mutate(fad_fished  = ifelse(commodity %in% fad_fished, 1, 0), 
         flow_binary = ifelse(quantity != 'NA', ifelse(quantity > 0, 1, 0)))

# exports currently include both exports and reexports (to indicate export streams/capacity)

########################## GOVERNANCE INDICATORS ###############################

wgi <- read.csv(here("raw_data", "governance", "wgi_indicators.csv"), stringsAsFactors = F) %>%
  clean_names() %>%
  rename("value" = x2018_yr2018) %>%
  select(country_name, country_code, series_name, value) %>%
  spread(series_name, value) %>%
  set_names("country","alpha_3","corruption","gov_eff","pol_stab","reg_qual","rule_law","accountability") %>%
  mutate_at(vars(3:8),funs(as.numeric)) %>%
  rowwise() %>%
  mutate(wgi_mean = mean(c(corruption, gov_eff, pol_stab, reg_qual, rule_law, accountability), na.rm = T)) %>%
  select(alpha_3, everything(), -country)

########################### TOURISM ####################################

tourism <- read.csv(here("raw_data", "tourism", "cto_2015_tourism.csv"), stringsAsFactors = F) %>%
  select(alpha_3, foreign_tourists)
  
########################### MERGING DATASETS ####################################

social_data <- iso %>%
  select(name_govt, alpha_3) %>%
  left_join(nutrition, by = "alpha_3") %>%
  left_join(wgi, by = "alpha_3") %>%
  left_join(trade_fish, by = "alpha_3") %>%
  left_join(tourism, by = "alpha_3") %>%
  replace(. == "NaN", NA) %>%
  mutate_all(na_if,"")

write.csv(trade_fish, here("data/fao_trade.csv"), row.names = F)
    
