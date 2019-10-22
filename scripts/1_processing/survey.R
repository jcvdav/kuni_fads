# Cleaning/coding FAD survey data

library(tidyverse)
library(here)

survey <- read.csv(here("raw_data","survey","survey.csv")) %>%
  set_names("time","email","respondent","country","set_yn","set_enf","set_type","whofish_yn","whofish_enf","whofish_type","howfish_yn","howfish_enf","howfish_type","nfads_pub","nfads_priv","nvessels_fads","nvessels_tot","comments") %>%
  select(-time, -email, -respondent, -comments) 

### when we have final survey data I will clean names, YN responses manually as needed 
