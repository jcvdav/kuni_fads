library("tidyverse")

DirectEmpl <- read.csv("raw_data/tourism/WTTC_DirectEmployment.csv", header = T, stringsAsFactors = F)
DirectGDP <- read.csv("raw_data/tourism/WTTC_DirectGDP.csv", header = T, stringsAsFactors = F)
TotalEmpl <- read.csv("raw_data/tourism/WTTC_TotalEmployment.csv", header = T, stringsAsFactors = F)
TotalGDP <- read.csv("raw_data/tourism/WTTC_TotalGDP.csv", header = T, stringsAsFactors = F)
iso <- read.csv("raw_data/iso_codes.csv") %>% 
  rename('country' = 'name')

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
