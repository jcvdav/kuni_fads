library(tidyverse)
library(here)
library(countrycode)

# All Marine Fish (data base from Julia's laptop)

AllMarineFish <- read.csv("raw_data/trade/FAO_species_allmarinefish_1976_2016.csv", header = T, stringsAsFactors = F)

AllMarineFish.tidy <- AllMarineFish %>% 
  gather("Year", "Quantity", 5:45) %>% 
  separate(Year,c("X" , "Year"), "X") %>% 
  separate(Quantity, c("Quantity", " F"), " F") %>% 
  rename("Country" = "Country..Country.") %>% 
  rename("Commodity" = "Commodity..Commodity.") %>% 
  rename("Flow" = "Trade.flow..Trade.flow.") %>% 
  filter(Country != "FAO. 2018. Fishery and Aquaculture Statistics. Global Fisheries commodities production and trade 1976-2016 (FishstatJ). In: FAO Fisheries and Aquaculture Department [online]. Rome. Updated 2018. www.fao.org/fishery/statistics/software/fishstatj/en") %>% 
  mutate(Quantity = recode(Quantity, "0 0" = "0")) %>% 
  filter(Year == "2014" | Year == "2015" | Year == "2016")

write.csv(AllMarineFish.tidy, file = "raw_data/trade/AllMarineFish.tidy.csv")

# FAO FAD fisheries data

FADFish <- read.csv("raw_data/trade/FAO_species_fad_1976_2016.csv", header = T, stringsAsFactors = F)

FADFish.tidy <- FADFish %>% 
  gather("Year", "Quantity", 5:45) %>% 
  separate(Year,c("X" , "Year"), "X") %>% 
  separate(Quantity, c("Quantity", " F"), " F") %>% 
  rename("Country" = "Country..Country.") %>% 
  rename("Commodity" = "Commodity..Commodity.") %>% 
  rename("Flow" = "Trade.flow..Trade.flow.") %>% 
  filter(Country != "FAO. 2018. Fishery and Aquaculture Statistics. Global Fisheries commodities production and trade 1976-2016 (FishstatJ). In: FAO Fisheries and Aquaculture Department [online]. Rome. Updated 2018. www.fao.org/fishery/statistics/software/fishstatj/en") %>% 
  mutate(Quantity = recode(Quantity, "0 0" = "0")) %>% 
  filter(Year == "2014" | Year == "2015" | Year == "2016")

View(FADFish.tidy)

write.csv(FADFish.tidy, file = "raw_data/trade/FADFish_tidy.csv")

#############################################################################################

# Defining FAD fisheries

fad_fished <- c("Albacore (=Longfin tuna), fresh or chilled", "Albacore (=Longfin tuna), frozen, nei", "Atlantic (Thunnus thynnus) and Pacific (Thunnus orientalis) bluefin tuna, frozen", "Euthynnus other than skipjack prep. or pres. not minced, nei", "Skipjack tuna, frozen", "Southern bluefin tuna (Thunnus maccoyii), live", "Tuna loins and fillets, frozen", "Tuna loins, prepared or preserved", "Tunas nei, frozen", "Tunas prepared or preserved, not minced, in oil", "Tunas prepared or preserved, not minced, nei", "Tunas, fresh or chilled, nei", "Yellowfin tuna, fresh or chilled", "Yellowfin tuna, frozen, nei", "Atlantic (Thunnus thynnus), Pacific (T.orientalis) bluefin tuna, live", "Atlantic (Thunnus thynnus)and Pacific (Thunnus orientalis) bluefin tuna, fresh or chilled", "Bigeye tuna, fresh or chilled", "Skipjack tuna, fresh or chilled", "Bigeye tuna, frozen, nei", "Dolphinfishes, fresh or chilled", "Dolphinfishes, frozen", "Miscellaneous pelagic fish fillets, frozen, nei", "Miscellaneous pelagic fish, fillets, fresh or chilled, nei", "Miscellaneous pelagic fish, nei, fresh or chilled", "Miscellaneous pelagic fish, nei, frozen", "Skipjack prepared or preserved, not minced, nei", "Southern bluefin tuna (Thunnus maccoyii), fresh or chilled", "Southern bluefin tuna (Thunnus maccoyii), frozen", "Tunas prepared or preserved, not minced, in airtight containers", "Tunas, bonitos, billfishes, fresh or chilled, nei", "Tunas, bonitos, billfishes, frozen, nei", "Tunas, flakes and grated, prepared or preserved", "Tuna loins and fillets, fresh or chilled", "Tuna meat, whether or not minced, frozen", "Tunas, bonitos, billfishes, meat, whether or not minced, frozen, nei", "Tunas, bonitos, billfishes, nei, minced, prepared or preserved", "Bonito (Sarda spp.), not minced, prepared or preserved, nei", "Miscellaneous pelagic fish nei, minced, prepared or preserved", "Skipjack, prepared or preserved, whole or in pieces, not minced, in oil", "Tunas nei, minced, prepared or preserved", "Yellowfin tuna, heads-off, etc., frozen", "Albacore (=Longfin tuna), gilled, gutted, frozen", "Albacore (=Longfin tuna), heads-off, etc., frozen", "Euthynnus excl. skipjack or stripe-bellied bonitos, fresh or chilled", "Euthynnus excl. skipjack or stripe-bellied bonitos, frozen", "Tunas, gilled, gutted, frozen, nei", "Yellowfin tuna, gilled, gutted, frozen", "Tunas, heads-off, etc., frozen, nei", "Miscellaneous pelagic fish nei, dried, whether or not salted", "Miscellaneous pelagic fish nei, fillets, dried, salted or in brine", "Marlins, fresh or chilled", "Marlins, frozen", "Tunas, bonitos, billfishes fillets, fresh or chilled, nei", "Tunas, bonitos, billfishes etc, fillets, frozen, nei")

# All tidy 
trade_fish <- read.csv(here("raw_data/trade/AllMarineFish.tidy.csv"), header = T, stringsAsFactors = F, na.strings = c("...", "-")) %>% 
  filter(Country != 'Totals') %>% 
  mutate(fad_fished  = ifelse(Commodity %in% fad_fished, 1, 0), 
         flow_binary = ifelse(Quantity != 'NA', ifelse(Quantity > 0, 1, 0))) %>% 
  select(-(c(X.1, X, X.F))) %>% 
  mutate(Country= ifelse(Country == "Netherlands Antilles", "Bonaire, Sint Eustatius and Saba", ifelse(Country == "CuraÃ§ao", 'Curaçao', Country))) %>% 
  mutate(alpha3 = countrycode(Country, "country.name", "iso3c"))
  
  
write.csv(trade_fish, here("data/fao_trade.csv"), row.names = F)
