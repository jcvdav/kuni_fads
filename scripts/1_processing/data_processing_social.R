### Cleaning and merging nutrition, WGI, and trade data

library(startR)
library(tidyverse)
library(here)
library(janitor)
library(countrycode)
library(scales)

# file with country names and alpha_3 codes
iso <- read.csv(here("raw_data", "iso_codes.csv"),
                stringsAsFactors = F,
                fileEncoding = "UTF-8-BOM") %>% 
  clean_names()

############################## NUTRITION  ####################################

nutrition <- read.csv(here("raw_data", "nutrition", "fao_fs_indicators.csv"),
                   stringsAsFactors = F,
                   fileEncoding = "UTF-8-BOM") %>% 
  clean_names() %>%
  filter(area != "Caribbean") %>%
  select(area,item,value) %>%
  spread(item, value) %>%
  set_names("country","energy_ad","sev_insecurity","undernourishment") %>%
  mutate(alpha_3 = countrycode(country, 'country.name', 'iso3c')) %>%
  select(alpha_3, energy_ad) %>%
  mutate(energy_ad = as.numeric(energy_ad))

############################## POVERTY ####################################

poverty <- read.csv(here("raw_data", "poverty", "poverty.csv"),
                   stringsAsFactors = F) %>% 
  clean_names() %>%
  select(alpha_3, poverty_rate)

########################### POPULATION DATA ####################################

pop <- read.csv(here("raw_data", "population", "pop_FAO.csv"), stringsAsFactors = F, fileEncoding = "UTF-8-BOM") %>%
  mutate(alpha_3 = countrycode(Area, 'country.name', 'iso3c')) %>% 
  select(-(Area)) %>% 
  spread(Year, Value) 

colnames <- c("alpha_3", "pop_2015", "pop_2016", "pop_2017", "pop_2018")

colnames(pop) <- colnames

pop <- pop %>% 
  mutate(mean_pop = (pop_2016+ pop_2017+ pop_2018)/3) %>% 
  select(alpha_3, mean_pop)

######################## FAO TRADE DATA ################################

# sum function for calculating aggregate quantities
my_sum <- function(x) {
  # If all values are NA, return NA
  if(sum(is.na(x)) == length(x)) {
    res <- 0
  } else {
    # If they are not, do the calculation
    res <- sum(x, na.rm = T)
  }
  # Return the result
  return(res)
}

# Defining fad-fished and fad-fished frozen and fresh commodities

fad_fished <- c("Albacore (=Longfin tuna), fresh or chilled", "Albacore (=Longfin tuna), frozen, nei", "Atlantic (Thunnus thynnus) and Pacific (Thunnus orientalis) bluefin tuna, frozen", "Euthynnus other than skipjack prep. or pres. not minced, nei", "Skipjack tuna, frozen", "Southern bluefin tuna (Thunnus maccoyii), live", "Tuna loins and fillets, frozen", "Tuna loins, prepared or preserved", "Tunas nei, frozen", "Tunas prepared or preserved, not minced, in oil", "Tunas prepared or preserved, not minced, nei", "Tunas, fresh or chilled, nei", "Yellowfin tuna, fresh or chilled", "Yellowfin tuna, frozen, nei", "Atlantic (Thunnus thynnus), Pacific (T.orientalis) bluefin tuna, live", "Atlantic (Thunnus thynnus)and Pacific (Thunnus orientalis) bluefin tuna, fresh or chilled", "Bigeye tuna, fresh or chilled", "Skipjack tuna, fresh or chilled", "Bigeye tuna, frozen, nei", "Dolphinfishes, fresh or chilled", "Dolphinfishes, frozen", "Miscellaneous pelagic fish fillets, frozen, nei", "Miscellaneous pelagic fish, fillets, fresh or chilled, nei", "Miscellaneous pelagic fish, nei, fresh or chilled", "Miscellaneous pelagic fish, nei, frozen", "Skipjack prepared or preserved, not minced, nei", "Southern bluefin tuna (Thunnus maccoyii), fresh or chilled", "Southern bluefin tuna (Thunnus maccoyii), frozen", "Tunas prepared or preserved, not minced, in airtight containers", "Tunas, bonitos, billfishes, fresh or chilled, nei", "Tunas, bonitos, billfishes, frozen, nei", "Tunas, flakes and grated, prepared or preserved", "Tuna loins and fillets, fresh or chilled", "Tuna meat, whether or not minced, frozen", "Tunas, bonitos, billfishes, meat, whether or not minced, frozen, nei", "Tunas, bonitos, billfishes, nei, minced, prepared or preserved", "Bonito (Sarda spp.), not minced, prepared or preserved, nei", "Miscellaneous pelagic fish nei, minced, prepared or preserved", "Skipjack, prepared or preserved, whole or in pieces, not minced, in oil", "Tunas nei, minced, prepared or preserved", "Yellowfin tuna, heads-off, etc., frozen", "Albacore (=Longfin tuna), gilled, gutted, frozen", "Albacore (=Longfin tuna), heads-off, etc., frozen", "Euthynnus excl. skipjack or stripe-bellied bonitos, fresh or chilled", "Euthynnus excl. skipjack or stripe-bellied bonitos, frozen", "Tunas, gilled, gutted, frozen, nei", "Yellowfin tuna, gilled, gutted, frozen", "Tunas, heads-off, etc., frozen, nei", "Miscellaneous pelagic fish nei, dried, whether or not salted", "Miscellaneous pelagic fish nei, fillets, dried, salted or in brine", "Marlins, fresh or chilled", "Marlins, frozen", "Tunas, bonitos, billfishes fillets, fresh or chilled, nei", "Tunas, bonitos, billfishes etc, fillets, frozen, nei")

fad_fished_ff <- c("Albacore (=Longfin tuna), fresh or chilled", "Albacore (=Longfin tuna), frozen, nei", "Atlantic (Thunnus thynnus) and Pacific (Thunnus orientalis) bluefin tuna, frozen", "Skipjack tuna, frozen", "Southern bluefin tuna (Thunnus maccoyii), live", "Tuna loins and fillets, frozen", "Tunas nei, frozen", "Tunas, fresh or chilled, nei", "Yellowfin tuna, fresh or chilled", "Yellowfin tuna, frozen, nei", "Atlantic (Thunnus thynnus), Pacific (T.orientalis) bluefin tuna, live", "Atlantic (Thunnus thynnus)and Pacific (Thunnus orientalis) bluefin tuna, fresh or chilled", "Bigeye tuna, fresh or chilled", "Skipjack tuna, fresh or chilled", "Bigeye tuna, frozen, nei", "Dolphinfishes, fresh or chilled", "Dolphinfishes, frozen", "Miscellaneous pelagic fish fillets, frozen, nei", "Miscellaneous pelagic fish, fillets, fresh or chilled, nei", "Miscellaneous pelagic fish, nei, fresh or chilled", "Miscellaneous pelagic fish, nei, frozen", "Southern bluefin tuna (Thunnus maccoyii), fresh or chilled", "Southern bluefin tuna (Thunnus maccoyii), frozen", "Tunas, bonitos, billfishes, fresh or chilled, nei", "Tunas, bonitos, billfishes, frozen, nei", "Tuna loins and fillets, fresh or chilled", "Tuna meat, whether or not minced, frozen", "Tunas, bonitos, billfishes, meat, whether or not minced, frozen, nei", "Yellowfin tuna, heads-off, etc., frozen", "Albacore (=Longfin tuna), gilled, gutted, frozen", "Albacore (=Longfin tuna), heads-off, etc., frozen", "Euthynnus excl. skipjack or stripe-bellied bonitos, fresh or chilled", "Euthynnus excl. skipjack or stripe-bellied bonitos, frozen", "Tunas, gilled, gutted, frozen, nei", "Yellowfin tuna, gilled, gutted, frozen", "Tunas, heads-off, etc., frozen, nei", "Marlins, fresh or chilled", "Marlins, frozen", "Tunas, bonitos, billfishes fillets, fresh or chilled, nei", "Tunas, bonitos, billfishes etc, fillets, frozen, nei")


# Importing fao trade data

trade_fao <- read.csv(here("raw_data/trade/AllMarineFish.tidy.csv"), header = T, stringsAsFactors = F, na.strings = c("...", "-")) %>% 
  clean_names() %>%
  select(-(c(x_1, x, x_f))) %>% 
  filter(country != "Totals - All") %>% 
  mutate(country= ifelse(country == "Netherlands Antilles", "Bonaire, Sint Eustatius and Saba", ifelse(country == "CuraÃ§ao", 'Curaçao', country))) %>% 
  mutate(alpha_3 = countrycode(country, "country.name", "iso3c"),
         fad_fished = ifelse(commodity %in% fad_fished, 1, 0), 
         fad_fished_ff = ifelse (commodity %in% fad_fished_ff,1,0)) %>% 
  # calculating total tonnes of every category of commodity per year and country (NAs as zero!)
  group_by(flow, year, alpha_3, fad_fished, fad_fished_ff) %>% 
  summarize(tonnes = my_sum(quantity)) %>% 
  ungroup() %>% 
  # generating a variable with type of commodities
  mutate(category = ifelse(fad_fished == 0 & fad_fished_ff == 0, 'no_fad_no_ff', ifelse(fad_fished == 1 & fad_fished_ff == 0, 'fad_no_ff', ifelse(fad_fished == 0 & fad_fished_ff == 1, 'no_fad_ff', 'fad_ff')))) %>% 
  select(-c(fad_fished, fad_fished_ff)) %>% 
  # generating a column for each kind of commodity per year, country and type of flow
  spread(category, tonnes, fill= 0) %>% 
  # calculating all marine, all fad and proportion of freash and frozen fad over all fad (NaN for zero over zero!)
  mutate(all_marine = fad_ff + fad_no_ff + no_fad_no_ff, 
         all_fad = fad_ff + fad_no_ff,
         ff_over_all_fad = fad_ff/all_fad) %>% 
  # calculating the mean over the last three years for each country
  group_by(flow, alpha_3) %>% 
  summarise(all_marine = mean(all_marine), all_fad = mean(all_fad), ff_over_all_fad = mean(ff_over_all_fad)) %>% 
  ungroup()

# extracting columns of all fad-products and proportion of ff fad products over all fad products only for export flows
exports <- trade_fao %>% 
  filter(flow == "Exports") %>% 
  select(alpha_3, exported_fad = all_fad, pp_ff_over_fad_exp = ff_over_all_fad) 

# combining with imports flows of interest (all marine)
trade <- trade_fao %>% 
  filter(flow == "Imports") %>% 
  select(alpha_3, imported_all_marine= all_marine) %>% 
  full_join(exports, by = 'alpha_3') %>% 
  full_join(pop, by = 'alpha_3') %>% 
# per capita calculations for ell exported fad-products and all impoerted marine products
  mutate(pc_imported_all = imported_all_marine/mean_pop, pc_exported_fad = exported_fad/mean_pop) %>% 
  select(alpha_3, pc_imported_all, pc_exported_fad, pp_ff_over_fad_exp) %>% 
  mutate(pp_ff_over_fad_exp = ifelse(pp_ff_over_fad_exp == 'NaN', 0 , pp_ff_over_fad_exp))
  
########################## GOVERNANCE INDICATORS ###############################

wgi <- read.csv(here("raw_data", "governance", "wgi_indicators.csv"), stringsAsFactors = F) %>%
  clean_names() %>%
  rename("value" = x2018_yr2018) %>%
  select(country_name, country_code, series_name, value) %>%
  spread(series_name, value) %>%
  set_names("country","alpha_3","wgi_corrupt","wgi_goveff","wgi_polstab","wgi_regqual","wgi_rulelaw","wgi_account") %>%
  mutate_at(.vars = vars(3:8),.funs = as.numeric) %>%
  rowwise() %>%
  mutate(wgi_mean = mean(c(wgi_corrupt, wgi_goveff, wgi_polstab, wgi_regqual, wgi_rulelaw, wgi_account), na.rm = T)) %>%
  select(alpha_3, everything(), -country)

########################### TOURISM ####################################

tourism <- read.csv(here("raw_data", "tourism", "cto_2015_tourism.csv"), stringsAsFactors = F) %>%
  group_by(alpha_3) %>%
  select("alpha_3", "foreign_tourists") %>% 
  summarize(tourists = sum(as.numeric(foreign_tourists, rm.na = T))) %>% 
  full_join(pop, by = 'alpha_3') %>% 
  mutate(pc_n_tourists = tourists/mean_pop) %>% 
  select(alpha_3, pc_n_tourists)

########################### SURVEY DATA ####################################

survey_clean <- read.csv(here("raw_data", "survey", "survey_clean.csv"), stringsAsFactors = F) %>%
  clean_names() %>%
  set_names("time","email","name","country","reg_set_yn","reg_set_enf_yn","reg_set_type","reg_whofish_yn","reg_whofish_enf_yn","reg_whofish_type","reg_howfish_yn","reg_howfish_enf_yn","reg_howfish_type","nfads_public","nfads_private","nvessels_fads","nvessels_tot","comments") %>%
  mutate(alpha_3 = countrycode(country, 'country.name', 'iso3c')) %>%
  mutate(alpha_3 = case_when(country == "Bonaire" ~ "BESB",
                             country == "St. Eustatius" ~ "BESE",
                             country == "Saba" ~ "BESS",
                             country == "Saint Martin" ~ "MAF",
                             TRUE ~ alpha_3)
          ) %>%
  select(country, alpha_3, reg_set_yn, reg_set_enf_yn, reg_set_type, reg_whofish_yn, reg_whofish_enf_yn, reg_whofish_type, reg_howfish_yn, reg_howfish_enf_yn, reg_howfish_type)

survey_govt <- survey_clean %>% mutate(reg_set_pres = case_when(reg_set_yn == "No" ~ 0,
                                            str_detect(reg_set_type, "Draft") ~ .5,
                                            str_detect(reg_set_type, "Formal") ~ 1,
                                            str_detect(reg_set_type, "formal") ~ 1),
                                      reg_whofish_pres = case_when(reg_whofish_yn == "No" ~ 0,
                                            str_detect(reg_whofish_type, "Draft") ~ .5,
                                            str_detect(reg_whofish_type, "Formal") ~ 1,
                                            str_detect(reg_whofish_type, "formal") ~ 1),
                                      reg_howfish_pres = case_when(reg_howfish_yn == "No" ~ 0,
                                            str_detect(reg_howfish_type, "Draft") ~ .5,
                                            str_detect(reg_howfish_type, "Formal") ~ 1,
                                            str_detect(reg_howfish_type, "formal") ~ 1)
  ) %>%
  mutate_at(.vars = vars(c(reg_set_enf_yn, reg_whofish_enf_yn, reg_howfish_enf_yn)),
            .funs = ~ case_when(. == "Yes" ~ 1,
                                . == "No" ~ .5,
                                is.na(.) ~ 0 # NAs mean no enforcement because no reg presence - don't use this df for graphing responses
            )) %>%
  mutate(reg_strength = 1/3 * reg_set_pres * reg_set_enf_yn + 1/3 * reg_whofish_pres * reg_whofish_enf_yn + 1/3 * reg_howfish_pres * reg_howfish_enf_yn
         ) %>%
  select(alpha_3, reg_strength)


survey_plot <- survey_clean %>%
  select(alpha_3, reg_set_yn, reg_set_enf_yn, reg_whofish_yn, reg_whofish_enf_yn, reg_howfish_yn, reg_howfish_enf_yn) %>%
  gather("question", "response", -alpha_3) %>%
  mutate(category = ifelse(grepl("enf", question), "Enforcement", "Existence"),
         reg_type = ifelse(grepl("set", question), "Setting MFADs",
                           ifelse(grepl("whofish", question), "Rights to use MFADs", "Fishing practices on MFADs"))
         ) %>%
  group_by(question, category, reg_type, response) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  group_by(question, category, reg_type) %>%
  mutate(n = sum(count)) %>%
  ungroup() %>%
  mutate(prop = count / n) %>%
  mutate(response = replace_na(response, "Did not respond"),
         category = fct_relevel(category, "Existence", "Enforcement"),
         reg_type = fct_relevel(reg_type, "Setting MFADs", "Rights to use MFADs"))

ggplot(survey_plot, aes(x = category, y = prop, fill = response)) +
  geom_col(color = "black") +
  facet_grid(~ reg_type) +
  theme_bw() +
  theme(legend.title = element_blank()) +
  labs(x = "", y = "Response frequency (n = 15)")  +
  scale_y_continuous(limits = c(0, 1)) +
  scale_fill_manual(values = c("gray", "red", "steelblue")) +
  #ggtheme_plot() +
  guides(fill = guide_legend(title = "Response"))

ggsave(plot = last_plot(),
       filename = here("img", "survey_gov.png"),
       width = 7,
       height = 3)

########################## MERGING DATASETS ###################################

social_data <- iso %>%
  select(name_govt, alpha_3) %>%
  distinct() %>%
  left_join(nutrition, by = "alpha_3") %>%
  left_join(poverty, by = "alpha_3") %>%
  left_join(trade, by = "alpha_3") %>%
  left_join(tourism, by = "alpha_3") %>% 
  left_join(survey_govt, by = "alpha_3") %>% 
  replace(. == "NaN", NA) %>%
  mutate_all(na_if, "")

data_scaled <- social_data %>%
  mutate_at(vars(-c(name_govt, alpha_3)), as.numeric) %>%
  mutate_if(is.numeric, rescale, to = c(0,1)) %>%
  mutate(energy_ad = 1 - energy_ad,
         score_govt = reg_strength,
         score_need = (energy_ad + poverty_rate) / 2,
         score_marketability = ((pc_imported_all + pc_n_tourists)/2 + (pc_exported_fad + pp_ff_over_fad_exp)/2)/2
        )

data_scaled2 <- data_scaled %>%
  select(alpha_3, score_govt, score_need, score_marketability) %>%
  mutate_if(is.numeric, rescale, to = c(0,1))
  
         
write.csv(social_data, here("data", "social_data.csv"), row.names = F)
write.csv(data_scaled, here("data", "data_scaled.csv"), row.names = F)
write.csv(data_scaled2, here("data", "data_scaled2.csv"), row.names = F)    
