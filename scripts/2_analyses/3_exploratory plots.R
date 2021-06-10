## Exploratory plotting
##########################################

# Load packages
library(startR)
library(here)
library(tidyverse)

# Load data

scaled_data <- read.csv(here("data", "data_scaled.csv"),
                        stringsAsFactors = F) %>%
  select(alpha_3, score_govt, score_wgi, score_need, score_marketability, n_fads, n_private, fads_per_totvessel) %>%
  mutate(p_private = n_private/n_fads)

cost_data <- read.csv(here("data", "country_level_cost_summary_statistics.csv"),
                      stringsAsFactors = F) %>% 
  mutate(score_cost = rescale(mean, to = c(0, 1))) %>% 
  select(ISO3, score_cost)

market_data <- read.csv(here("data", "social_data.csv"),
                      stringsAsFactors = F) %>% 
  select(name_govt, alpha_3, mean_pop, Exports_percap, Imports_percap, pc_n_tourists) #%>%
#  mutate_if(is.numeric, rescale, to = c(0,1))

## Combine data
data <- cost_data %>% 
  left_join(scaled_data, by = c("ISO3" = "alpha_3"))


# Exploring relationships among variables

## Preliminary - comparing our 2 governance indicators - WGI vs. survey data (for islands with both)
ggplot(data, aes(x = score_wgi, y = score_govt)) +
  geom_point(aes(size = n_fads, color = p_private)) +
  # geom_text(aes(label=ISO3), size = 2) +
  theme_bw()
# can see some countries that don't have FAD regulations bc they don't have FADs (ABW, BHS) but actually have high governance capacity
# maybe instead of number of FADs here, classify as no fads, public, private?


## Cost and governance

### WGI ~ cost: somewhat positive relationship?
ggplot(data, aes(x = score_cost, y = score_wgi)) +
  geom_text(aes(label=ISO3), size = 2) +
  geom_smooth(method = "lm") +
  theme_bw()

### Govt survey ~ cost: no clear trends (but potentially interesting to look at countries?)
ggplot(data, aes(x = score_cost, y = score_govt)) +
  geom_text(aes(label=ISO3), size = 2) +
  geom_smooth(method = "lm") +
  theme_bw()

### Cost ~ WGI ~ FADs
ggplot(data, aes(x = score_cost, y = score_wgi)) +
  geom_point(aes(size = fads_per_totvessel)) +
  theme_bw()


## FAD numbers (FADs per total number of SSF vessels)
# note - maybe FADs per km coastline would be more informative...?

### FADs ~ WGI
ggplot(data, aes(x = score_wgi, y = fads_per_totvessel)) +
  geom_text(aes(label=ISO3), size = 2) +
  ylim(0,.75) +
  theme_bw() 

ggplot(data, aes(x = score_govt, y = fads_per_totvessel)) +
  geom_text(aes(label=ISO3), size = 2) +
  ylim(0,.75) +
  theme_bw() 

### FADs ~ cost
ggplot(data, aes(x = score_cost, y = fads_per_totvessel)) +
  geom_text(aes(label=ISO3), size = 2) +
  ylim(0,.75) +
  theme_bw() 

### FADs ~ marketability
ggplot(data, aes(x = score_marketability, y = fads_per_totvessel)) +
  geom_text(aes(label=ISO3), size = 2) +
  geom_smooth(method = "lm") +
  #ylim(0,.75) +
  theme_bw() 

### FADs ~ need
ggplot(data, aes(x = score_need, y = fads_per_totvessel)) +
  geom_text(aes(label=ISO3), size = 2) +
  ylim(0,.75) +
  theme_bw() 


## Governance and public vs. private
ggplot(data, aes(x = score_govt, y = p_private)) +
  # geom_point(aes(size = n_fads, color = p_private)) +
  geom_text(aes(label=ISO3), size = 2) +
  theme_bw()


## Need 

### Need ~ WGI: interesting somewhat negative correlation?
ggplot(data, aes(x = score_need, y = score_wgi)) +
  geom_text(aes(label=ISO3), size = 2) +
  geom_smooth(method = "lm") +
  theme_bw()

### Need ~ govt survey: less clear trends
ggplot(data, aes(x = score_need, y = score_govt)) +
  geom_text(aes(label=ISO3), size = 2) +
  theme_bw()

#Marketability

##More tourists = more imports. Except for Bermuda and Barbados, maybe because of large expat communities?
ggplot(market_data, aes(x = pc_n_tourists, y = Imports_percap, label = alpha_3)) +
  geom_point(aes(size = Exports_percap)) +
  geom_smooth(method = "lm") +
  xlim(0,11000) +
  geom_text(size=3, position=position_jitter(width=1,height=1)) +
  theme_bw() 