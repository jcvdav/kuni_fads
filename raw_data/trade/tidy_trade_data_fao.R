library(tidyverse)

#All Marine Fish

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
  
#FAD Species

