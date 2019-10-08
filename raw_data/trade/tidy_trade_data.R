library(tidyverse)
library(here)
library(dyplr)

# Importing and reshaping trade data in USD
trade_usd <- read.csv(here("raw_data/trade/FAO_species_trade_value_2010_2017.csv"), header = T, stringsAsFactors = F) %>% 
  rename( 'country' = 'Land.Area', "flow" = "Trade.flow", "commodity" = "Commodity") %>% 
  gather(year, usd, c(seq(4,39, 2))) %>% 
  select(country, flow, year, commodity, usd) %>% 
  mutate(year = ifelse(year == 'X2017', 2017, ifelse(year == 'X2016', 2016, ifelse(year == 'X2015', 2015, ifelse(year == 'X2014', 2014, ifelse(year == 'X2013', 2013, ifelse(year == 'X2012', 2012, ifelse(year == 'X2011', 2011, ifelse(year == 'X2010', 2010, ifelse(year == 'X2009', 2009, ifelse(year == 'X2008', 2008, ifelse(year == 'X2007', 2007, ifelse(year == 'X2006', 2006, ifelse(year == 'X2005', 2005, ifelse(year == 'X2004', 2004, ifelse(year == 'X2003', 2003, ifelse(year == 'X2002', 2002, ifelse(year == 'X2001', 2001,2000 ))))))))))))))))))

# Importing and reshaping trade data in tonnes
trade_ton <- read.csv(here("raw_data/trade/FAO_species_trade_quantity_2010_2017.csv"), header = T, stringsAsFactors = F) %>% 
  rename( 'country' = 'Land.Area', "flow" = "Trade.flow", "commodity" = "Commodity") %>% 
  gather(year, ton, c(seq(4,39, 2))) %>% 
  select(country, flow, year, commodity, ton) %>% 
  mutate(year = ifelse(year == 'X2017', 2017, ifelse(year == 'X2016', 2016, ifelse(year == 'X2015', 2015, ifelse(year == 'X2014', 2014, ifelse(year == 'X2013', 2013, ifelse(year == 'X2012', 2012, ifelse(year == 'X2011', 2011, ifelse(year == 'X2010', 2010, ifelse(year == 'X2009', 2009, ifelse(year == 'X2008', 2008, ifelse(year == 'X2007', 2007, ifelse(year == 'X2006', 2006, ifelse(year == 'X2005', 2005, ifelse(year == 'X2004', 2004, ifelse(year == 'X2003', 2003, ifelse(year == 'X2002', 2002, ifelse(year == 'X2001', 2001,2000 ))))))))))))))))))

# Joining value and quantity per country, commodity, flow and year

trade <- trade_ton %>% 
  full_join(trade_usd, by = c("country", "flow", "year", "commodity"))

# Importing iso names
iso <- read.csv(here("raw_data/iso_codes.csv")) %>% 
  rename('country' = 'name', name_govt = "ï..name_govt")






DirectEmpl_long <- DirectEmpl %>% 
  gather('X1995':'X2029', key = year, value = "Thousands of jobs") %>% 
  rename('value' = "Thousands of jobs") %>% 
  rename('measure' = 'Direct._employment') %>% 
  separate(year,c("X" , "year"), "X") %>% 
  select(-c(X)) %>% 
  left_join(iso, by="country")


DirectGDP_long <- DirectGDP %>% 
  gather('X1995':'X2029', key = year, value = "Local currency in bn (Nominal prices)") %>% 
  rename('value' = "Local currency in bn (Nominal prices)") %>% 
  rename('measure' = 'Direct._GDP') %>% 
  separate(year,c("X" , "year"), "X") %>% 
  select(-c(X)) %>% 
  left_join(iso, by="country")

TotalEmpl_long <- TotalEmpl %>% 
  gather('X1995':'X2029', key = year, value = "Thousands of jobs") %>% 
  rename('value' = "Thousands of jobs") %>% 
  rename('measure' = 'Total_Employment') %>% 
  separate(year,c("X" , "year"), "X") %>% 
  select(-c(X)) %>% 
  left_join(iso, by="country")

TotalGDP_long <- TotalGDP %>% 
  gather('X1995':'X2029', key = year, value = "Local currency in bn (Nominal prices)") %>% 
  rename('value' = "Local currency in bn (Nominal prices)") %>% 
  rename('measure' = 'Total_GDP') %>% 
  separate(year,c("X" , "year"), "X") %>% 
  select(-c(X)) %>% 
  left_join(iso, by="country")
