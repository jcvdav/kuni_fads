## Exploratory plotting
##########################################

# Load packages
library(startR)
library(here)
library(tidyverse)

# Load data

scaled_data <- read.csv(here("data", "data_scaled.csv"),
                        stringsAsFactors = F) %>%
  select(alpha_3, score_regs = score_govt, score_wgi, score_need, score_marketability, n_fads, n_private, fads_per_totvessel) %>%
  mutate(p_private = n_private/n_fads)

unscaled_data <- read.csv(here("data", "data_unscaled.csv"),
                          stringsAsFactors = F)

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
  left_join(scaled_data, by = c("ISO3" = "alpha_3")) %>%
  mutate(fad_category = case_when(n_fads == 0 ~ "None",
                                  p_private >=.5 ~ "Mostly private",
                                  p_private <.5 ~ "Mostly public",
                                  ISO3 == "DOM" ~ "Mostly private",
                                  ISO3 == "HTI" ~ "Mostly public"))


# Exploring relationships among variables

## Preliminary - comparing our 2 governance indicators - WGI vs. survey data (for islands with both)
ggplot(filter(data, n_fads != 0), aes(x = score_wgi, y = score_regs, label = ISO3)) +
  geom_point(aes(size = fads_per_totvessel, color = fad_category)) +
  geom_text(size = 3, position = position_jitter(width = .05, height = .05)) +
  geom_smooth(method = "lm") +
  theme_bw()
# can see some countries that don't have FAD regulations bc they don't have FADs (ABW, BHS) but actually have high governance capacity
# no clear link between governance scores and typology
# positive correlation between two gov. indicators if those without FADs are removed


## Cost and governance 

# ### WGI ~ cost: somewhat positive relationship?
# ggplot(data, aes(x = score_cost, y = score_wgi)) +
#   geom_text(aes(label=ISO3), size = 2) +
#   geom_smooth(method = "lm") +
#   theme_bw()
# 
# ### Govt survey ~ cost: no clear trends (but potentially interesting to look at countries?)
# ggplot(data, aes(x = score_cost, y = score_regs)) +
#   geom_text(aes(label=ISO3), size = 2) +
#   geom_smooth(method = "lm") +
#   theme_bw()
# 
# ### Cost ~ WGI ~ FADs
# ggplot(data, aes(x = score_cost, y = score_wgi)) +
#   geom_point(aes(size = fads_per_totvessel)) +
#   theme_bw()
# 

## FAD numbers (FADs per total number of SSF vessels)
# note - maybe FADs per km coastline would be more informative...?

### FADs ~ WGI
ggplot(data %>% filter(ISO3 != "HTI" & n_fads > 0), aes(x = score_wgi, y = fads_per_totvessel)) +
  geom_text(aes(label=ISO3), size = 2) +
  geom_smooth(method = "lm") +
  ylim(-.5,2) +
  theme_bw() 
# Just playing around - without Haiti and without countries with no FADs, negative relationship

# ggplot(data, aes(x = score_regs, y = fads_per_totvessel)) +
#   geom_text(aes(label=ISO3), size = 2) +
#   ylim(0,.75) +
#   theme_bw() 

### FADs ~ cost
ggplot(data, aes(x = score_cost, y = fads_per_totvessel)) +
  geom_text(aes(label=ISO3), size = 2) +
  geom_smooth(method = "lm") + 
  ylim(-.25, 1) +
  theme_bw()
# 
# ### FADs ~ marketability
# ggplot(data, aes(x = score_marketability, y = fads_per_totvessel)) +
#   geom_text(aes(label=ISO3), size = 2) +
#   geom_smooth(method = "lm") +
#   #ylim(0,.75) +
#   theme_bw() 
# 
# ### FADs ~ need
# ggplot(data, aes(x = score_need, y = fads_per_totvessel)) +
#   geom_text(aes(label=ISO3), size = 2) +
#   geom_smooth(method = "lm") +
#   ylim(-.25, 1) +
#   theme_bw() 


## Governance and public vs. private
# ggplot(data, aes(x = score_regs, y = p_private)) +
#   # geom_point(aes(size = n_fads, color = p_private)) +
#   geom_text(aes(label=ISO3), size = 2) +
#   theme_bw()
# 
# ggplot(data, aes(x = score_wgi, y = p_private)) +
#   # geom_point(aes(size = n_fads, color = p_private)) +
#   geom_text(aes(label=ISO3), size = 2) +
#   theme_bw()


## Need 

### Need ~ WGI: interesting somewhat negative correlation?
ggplot(data, aes(x = score_need, y = score_wgi)) +
  geom_text(aes(label=ISO3), size = 2) +
  geom_smooth(method = "lm") +
  theme_bw()
# higher need in lower WGI islands - not surprising, but important in deciding where to put FADs

# ### Need ~ govt survey: less clear trends
# ggplot(data, aes(x = score_need, y = score_govt)) +
#   geom_text(aes(label=ISO3), size = 2) +
#   theme_bw()

#Marketability

##More tourists = more imports. Except for Bermuda and Barbados, maybe because of large expat communities?
ggplot(market_data, aes(x = pc_n_tourists, y = Imports_percap, label = alpha_3)) +
  geom_point(aes(size = Exports_percap)) +
  geom_smooth(method = "lm") +
  xlim(0,11000) +
  geom_text(size=3, position=position_jitter(width=1,height=1)) +
  theme_bw() 

# Revised biplot (Fig. 2)
ggplot(filter(data, score_marketability >= 0 & score_cost >= 0), aes(x = score_marketability, y = score_cost, label = ISO3)) +
  geom_point(aes(size = score_need, color = score_regs)) +
  geom_text(size=3) +
  xlim(0,1) +
  ylim(0,1) +
  theme_bw() 

# Without cost...
ggplot(data, aes(x = score_marketability, y = score_regs, label = ISO3)) +
  geom_point(alpha = 0.5, aes(size = score_need, color = score_need)) +
  geom_text(size=3) +
  xlim(0,1) +
  ylim(0,1) +
  geom_hline(y = 0) +
  theme_minimal()



