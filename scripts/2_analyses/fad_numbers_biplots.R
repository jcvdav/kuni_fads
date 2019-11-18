### Test plots of FAD numbers vs. governance indicators

library(startR)
library(tidyverse)
library(here)
library(janitor)

fad_numbers <- fao_fs <- read.csv(here("raw_data", "fad_data", "fads_current.csv"), stringsAsFactors = F) %>% 
  clean_names() %>%
  select(alpha_3, n_fads, n_private, n_public, vessels_fad, vessels_tot) %>%
  mutate(fads_per_vessel = n_fads/vessels_tot,
         privfads_per_vessel = n_private/vessels_tot)

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

dataset <- fad_numbers %>%
  filter(alpha_3 != "DOM") %>%
  left_join(wgi, by = "alpha_3")

ggplot(dataset, aes(x = wgi_mean, y = n_fads)) +
  geom_point()

ggplot(dataset, aes(x = wgi_mean, y = fads_per_vessel)) +
  geom_point() +
  ylim(0,.2)

ggsave(plot = last_plot(),
       filename = here("img", "fads_vs_wgimean.png"),
       width = 4,
       height = 3)

ggplot(dataset, aes(x = wgi_mean, y = privfads_per_vessel)) +
  geom_point() +
  ylim(0,.2)
