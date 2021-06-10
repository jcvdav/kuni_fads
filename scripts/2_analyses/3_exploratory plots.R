## Exploratory plotting
##########################################

# Load packages
library(startR)
library(here)
library(tidyverse)

# Load data

scaled_data <- read.csv(here("data", "data_scaled.csv"),
                        stringsAsFactors = F) %>%
  select(alpha_3, score_govt, score_wgi, score_need, score_marketability, n_fads, fads_per_totvessel)

cost_data <- read.csv(here("data", "country_level_cost_summary_statistics.csv"),
                      stringsAsFactors = F) %>% 
  mutate(score_cost = rescale(mean, to = c(0, 1))) %>% 
  select(ISO3, score_cost)

## Combine data
data <- cost_data %>% 
  left_join(scaled_data, by = c("ISO3" = "alpha_3"))


# Exploring relationships among variables

## Governance indicators - WGI vs. survey data (for islands with both)
ggplot(data, aes(x = score_wgi, y = score_govt)) +
  geom_text(aes(label=ISO3), size = 2) +
  theme_bw()

## Cost and governance
ggplot(data, aes(x = score_cost, y = score_wgi)) +
  geom_text(aes(label=ISO3), size = 2) +
  theme_bw()

ggplot(data, aes(x = score_cost, y = score_govt)) +
  geom_text(aes(label=ISO3), size = 2) +
  theme_bw()

## FAD numbers
ggplot(data, aes(x = score_wgi, y = fads_per_totvessel)) +
  geom_text(aes(label=ISO3), size = 2) +
  ylim(0,1) +
  theme_bw() 

ggplot(data, aes(x = score_cost, y = fads_per_totvessel)) +
  geom_text(aes(label=ISO3), size = 2) +
  ylim(0,1) +
  theme_bw() 

ggplot(data, aes(x = score_marketability, y = fads_per_totvessel)) +
  geom_text(aes(label=ISO3), size = 2) +
  ylim(0,1) +
  theme_bw() 

## Need 

ggplot(data, aes(x = score_need, y = score_wgi)) +
  geom_text(aes(label=ISO3), size = 2) +
  theme_bw()

ggplot(data, aes(x = score_need, y = score_govt)) +
  geom_text(aes(label=ISO3), size = 2) +
  theme_bw()

ggplot(data, aes(x = score_need, y = score_cost)) +
  geom_text(aes(label=ISO3), size = 2) +
  theme_bw()
