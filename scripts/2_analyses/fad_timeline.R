### Timeline data + figure

library(startR)
library(tidyverse)
library(here)
library(janitor)
library(countrycode)

timeline <- read.csv(here("raw_data", "fad_data", "fads_timeline.csv"),
                      stringsAsFactors = F,
                      fileEncoding = "UTF-8-BOM") %>% 
  clean_names() %>%
  mutate(n_fads = ifelse(n_fads >500, 500, n_fads),
         zero_yn = ifelse(n_fads == 0, "zero", "nonzero")
         )

ggplot(timeline, aes(x = year, y = name, group = name)) +
  geom_jitter(aes(size = n_fads, colour = zero_yn), height = 0, width = 0.5, shape = 16, alpha = 0.5) +
  #scale_size(breaks = c(0,100,500)) +
  #scale_fill_gradient(low = "#FFFFFF", high = "#003366", aesthetics = "fill") +
  #scale_colour_manual()
  theme_bw() +
  labs(x = "Year", y = "country")


ggsave(plot = timeline, filename = here("img/timeline_new.tiff"), width = 8, height = 6)