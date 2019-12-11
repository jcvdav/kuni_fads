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
         presence = ifelse(n_fads == 0, "absent", "present")
         ) %>%
  filter(!is.na(presence))

ggplot(timeline, aes(x = year, y = reorder(name, desc(name)))) +
  geom_point(aes(size = n_fads, shape = presence), alpha = 0.5, color = "darkslategray4", height = 0, width = 0.5) +
  #scale_size(breaks = c(0,100,500)) +
  scale_size_continuous(range = c(1,10)) +
  #scale_colour_manual(values = c("darkslategray4","darkslategray4")) +
  scale_shape_manual(values = c(21,16)) +
  #scale_alpha_manual(values = c(1,.5)) +
  theme_bw() +
  labs(x = "", y = "", shape = "MFAD Presence", size = "Num. MFADs")

ggsave(plot = last_plot(), filename = here("img/timeline_new.png"), width = 6, height = 5)
