### Cleaning and merging nutrition, WGI, and trade data

library(tidyverse)
library(here)
library(janitor)
library(countrycode)
library(scales)

# file with country names and alpha_3 codes
iso <- read.csv(here("raw_data", "iso_codes.csv"), stringsAsFactors = F)

############################## NUTRITION + FOOD SECURITY ####################################
fao_fs <- fao_fs <- read.csv(here("raw_data", "nutrition", "fao_fs_indicators.csv"), stringsAsFactors = F) %>% 
  clean_names() %>%
  filter(area != "Caribbean") %>%
  select(area,item,value) %>%
  spread(item, value) %>%
  set_names("country","energy_ad","sev_insecurity","undernourishment") %>%
  mutate(alpha_3 = countrycode(country, 'country.name', 'iso3c')) %>%
  select(alpha_3, everything(), -country)

genus_intake <- read.csv(here("raw_data", "nutrition", "genus_intake.csv"), stringsAsFactors = F) %>% 
  clean_names() %>% # all from 2011
  mutate(calories_pf = calories_pelagicfish / calories, # calculating proportion of calories obtained from pelagic fish
         protein_pf = protein_pelagic_fish / protein) %>% # calculating proportion of protein obtained from pelagic fish
  select(country,calories_pf,protein_pf) %>%
  mutate(country= ifelse(country == "Netherlands Antilles", "Bonaire, Sint Eustatius and Saba", country)) %>% 
  mutate(alpha_3 = countrycode(country, 'country.name', 'iso3c')) %>%
  select(alpha_3, everything(), -country)

# selected nutrition variables:
nutrition <- fao_fs %>%
  select(alpha_3, energy_ad) %>% 
  full_join(genus_intake, by = "alpha_3") %>%
  rowwise() %>%
  mutate(reliance_pf = mean(c(calories_pf, protein_pf), na.rm = FALSE))

######################## FAO TRADE DATA ################################

# sum function for calculating aggregate quantities
my_sum <- function(x) {
  # If all values are NA, return NA
  if(sum(is.na(x)) == length(x)) {
    res <- NA
  } else {
    # If they ar enot, do the calculation
    res <- sum(x, na.rm = T)
  }
  # Return the result
  return(res)
}

trade_fish <- read.csv(here("raw_data/trade/AllMarineFish.tidy.csv"), header = T, stringsAsFactors = F, na.strings = c("...", "-")) %>% 
  clean_names() %>%
  select(-(c(x_1, x, x_f))) %>% 
  filter(country != 'Totals') %>% 
#  mutate(flow = ifelse(flow == "Reexports", "Exports", flow)) %>% # grouping reexports with exports (see note below)
  mutate(country= ifelse(country == "Netherlands Antilles", "Bonaire, Sint Eustatius and Saba", ifelse(country == "CuraÃ§ao", 'Curaçao', country))) %>% 
  mutate(alpha_3 = countrycode(country, "country.name", "iso3c"))

# calculating 3-year (2014-2016) averages of imports, exports, and production quantity of all fish products
trade_sf <- trade_fish %>%
  group_by(alpha_3, flow, year) %>%
  summarize(quantity_sf = my_sum(quantity)) %>%
  group_by(alpha_3, flow) %>%
  summarize(quantity_sf = mean(quantity_sf, na.rm = T)) %>%
  spread(flow, quantity_sf) %>%
  set_names("alpha_3","exports_fish","imports_fish","production_fish", "reexports_fish") %>%
  mutate(exports_fish_yn = ifelse(is.na(exports_fish) | exports_fish == 0, 0, 1),
         imports_fish_yn = ifelse(is.na(imports_fish) | imports_fish == 0, 0, 1),
         reexports_fish_yn = ifelse(is.na(reexports_fish) | reexports_fish == 0, 0, 1))
  
# calculating 3-year (2014-2016) averages of imports, exports, and production quantity of products from potential FAD species
fad_fished <- c("Albacore (=Longfin tuna), fresh or chilled", "Albacore (=Longfin tuna), frozen, nei", "Atlantic (Thunnus thynnus) and Pacific (Thunnus orientalis) bluefin tuna, frozen", "Euthynnus other than skipjack prep. or pres. not minced, nei", "Skipjack tuna, frozen", "Southern bluefin tuna (Thunnus maccoyii), live", "Tuna loins and fillets, frozen", "Tuna loins, prepared or preserved", "Tunas nei, frozen", "Tunas prepared or preserved, not minced, in oil", "Tunas prepared or preserved, not minced, nei", "Tunas, fresh or chilled, nei", "Yellowfin tuna, fresh or chilled", "Yellowfin tuna, frozen, nei", "Atlantic (Thunnus thynnus), Pacific (T.orientalis) bluefin tuna, live", "Atlantic (Thunnus thynnus)and Pacific (Thunnus orientalis) bluefin tuna, fresh or chilled", "Bigeye tuna, fresh or chilled", "Skipjack tuna, fresh or chilled", "Bigeye tuna, frozen, nei", "Dolphinfishes, fresh or chilled", "Dolphinfishes, frozen", "Miscellaneous pelagic fish fillets, frozen, nei", "Miscellaneous pelagic fish, fillets, fresh or chilled, nei", "Miscellaneous pelagic fish, nei, fresh or chilled", "Miscellaneous pelagic fish, nei, frozen", "Skipjack prepared or preserved, not minced, nei", "Southern bluefin tuna (Thunnus maccoyii), fresh or chilled", "Southern bluefin tuna (Thunnus maccoyii), frozen", "Tunas prepared or preserved, not minced, in airtight containers", "Tunas, bonitos, billfishes, fresh or chilled, nei", "Tunas, bonitos, billfishes, frozen, nei", "Tunas, flakes and grated, prepared or preserved", "Tuna loins and fillets, fresh or chilled", "Tuna meat, whether or not minced, frozen", "Tunas, bonitos, billfishes, meat, whether or not minced, frozen, nei", "Tunas, bonitos, billfishes, nei, minced, prepared or preserved", "Bonito (Sarda spp.), not minced, prepared or preserved, nei", "Miscellaneous pelagic fish nei, minced, prepared or preserved", "Skipjack, prepared or preserved, whole or in pieces, not minced, in oil", "Tunas nei, minced, prepared or preserved", "Yellowfin tuna, heads-off, etc., frozen", "Albacore (=Longfin tuna), gilled, gutted, frozen", "Albacore (=Longfin tuna), heads-off, etc., frozen", "Euthynnus excl. skipjack or stripe-bellied bonitos, fresh or chilled", "Euthynnus excl. skipjack or stripe-bellied bonitos, frozen", "Tunas, gilled, gutted, frozen, nei", "Yellowfin tuna, gilled, gutted, frozen", "Tunas, heads-off, etc., frozen, nei", "Miscellaneous pelagic fish nei, dried, whether or not salted", "Miscellaneous pelagic fish nei, fillets, dried, salted or in brine", "Marlins, fresh or chilled", "Marlins, frozen", "Tunas, bonitos, billfishes fillets, fresh or chilled, nei", "Tunas, bonitos, billfishes etc, fillets, frozen, nei")
trade_fads <- trade_fish %>%
  mutate(fad_fished  = ifelse(commodity %in% fad_fished, 1, 0)) %>%
  filter(fad_fished == 1) %>%
  group_by(alpha_3, flow, year) %>%
  summarize(quantity_fad = my_sum(quantity)) %>%
  group_by(alpha_3, flow) %>%
  summarize(quantity_fad = mean(quantity_fad, na.rm = T)) %>%
  spread(flow, quantity_fad) %>%
  set_names("alpha_3","exports_fad","imports_fad","production_fad", "reexports_fad") %>%
  replace(. == "NaN", NA) 
  # %>%
  # mutate(exports_fad_yn = ifelse(is.na(exports_fad) | exports_fad == 0, 0, 1),
  #        imports_fad_yn = ifelse(is.na(imports_fad) | imports_fad == 0, 0, 1),
  #        reexports_fad_yn = ifelse(is.na(reexports_fad) | reexports_fad == 0, 0, 1))

# merging into single trade dataset
trade <- trade_sf %>%
  left_join(trade_fads, by = "alpha_3") %>%
  mutate(trade_def_fad = imports_fad - exports_fad) %>%
  select(alpha_3, trade_def_fad, exports_fish)

########################## GOVERNANCE INDICATORS ###############################

wgi <- read.csv(here("raw_data", "governance", "wgi_indicators.csv"), stringsAsFactors = F) %>%
  clean_names() %>%
  rename("value" = x2018_yr2018) %>%
  select(country_name, country_code, series_name, value) %>%
  spread(series_name, value) %>%
  set_names("country","alpha_3","wgi_corrupt","wgi_goveff","wgi_polstab","wgi_regqual","wgi_rulelaw","wgi_account") %>%
  mutate_at(vars(3:8),funs(as.numeric)) %>%
  rowwise() %>%
  mutate(wgi_mean = mean(c(wgi_corrupt, wgi_goveff, wgi_polstab, wgi_regqual, wgi_rulelaw, wgi_account), na.rm = T)) %>%
  select(alpha_3, everything(), -country)

########################### TOURISM ####################################

# currently this produces all NAs - needs to be adjusted with BES average

tourism <- read.csv(here("raw_data", "tourism", "cto_2015_tourism.csv"), stringsAsFactors = F) %>%
  group_by(alpha_3) %>%
  mutate_all(na_if,"") %>%
  summarize(tourists = sum(as.numeric(foreign_tourists, rm.na = T))) %>%
  select(alpha_3, tourists)
  
########################### SURVEY DATA ####################################

# survey <- read.csv(here("raw_data", "survey", "survey_clean.csv"), stringsAsFactors = F) %>%
#   clean_names() %>%
#   setNames("time","email","name","country","reg_set_yn","reg_set_enf_yn","reg_set_type","reg_whofish_yn","reg_whofish_enf_yn","reg_whofish_type","reg_howfish_yn","reg_howfish_enf_yn","reg_howfish_type","nfads_public","nfads_private","nvessels_fads","nvessels_tot","comments") %>%
#   mutate(alpha_3 = countrycode(country, 'country.name', 'iso3c')) %>%
#   select(country, reg_set_yn, reg_set_enf_yn, reg_whofish_yn, reg_whofish_enf_yn, reg_howfish_yn, reg_howfish_enf_yn) %>%
#   mutate_all(ifelse("Yes", 1, 
#                     ifelse("No", 0, "NA")))


########################## MERGING DATASETS ###################################

social_data <- iso %>%
  select(name_govt, alpha_3) %>%
  distinct() %>%
  left_join(nutrition, by = "alpha_3") %>%
  left_join(wgi, by = "alpha_3") %>%
  left_join(trade, by = "alpha_3") %>%
  left_join(tourism, by = "alpha_3") %>% # need to distinguish among BES tourism numbers, right now has numbers for different islands but no island reference
  replace(. == "NaN", NA) %>%
  mutate_all(na_if,"")

data_scaled <- social_data %>%
  mutate_at(vars(3:27), as.numeric) %>%
  mutate_if(is.numeric, rescale, to = c(0,1)) %>%
  mutate(score_nutrit = 1/2 * reliance_pf + 1/2 * energy_ad,
         score_govt = wgi_mean # placeholder until survey data is calculated
         # score_econ =
           )

# write.csv(data_scaled, here("raw_data/data_scaled.csv"), row.names = F)
    
