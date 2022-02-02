## Exploratory plotting
##########################################

# Load packages
library(startR)
library(here)
library(tidyverse)
library(scales)
library(ggrepel)
library(janitor)

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
  select(name_govt, alpha_3, Exports_percap, Imports_percap, pc_n_tourists) #%>%
#  mutate_if(is.numeric, rescale, to = c(0,1))

iso <- read.csv(here("raw_data", "iso_codes.csv"),
                stringsAsFactors = F,
                fileEncoding = "UTF-8-BOM") %>% 
  clean_names() %>%
  mutate_at("name", str_replace, " and", " &") %>%
  mutate_at("name", str_replace, "Saint", "St.")

## Combine data
data <- cost_data %>% 
  left_join(scaled_data, by = c("ISO3" = "alpha_3")) %>%
  left_join(iso, by = c("ISO3" = "alpha_3")) %>%
  mutate(fad_category = case_when(n_fads == 0 ~ "None",
                                  p_private >=.5 ~ "Mostly private",
                                  p_private <.5 ~ "Mostly public",
                                  ISO3 == "DOM" ~ "Mostly private",
                                  ISO3 == "HTI" ~ "Mostly public"))

results <- data %>%
  select(State = name, "Biophysical suitibility & cost" = score_cost, "MFAD regulatory strength" = score_regs, "Social need" = score_need, "Catch marketability" = score_marketability)
write.csv(results, here("data", "results_table.csv"), row.names = F)


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
ggplot(data, aes(x = score_need, y = fads_per_totvessel)) +
  geom_text(aes(label=ISO3), size = 2) +
  geom_smooth() +
  ylim(-.25, 1) +
  theme_bw()


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
ggplot(filter(data, score_regs >= 0 & score_cost >= 0), aes(x = score_regs, y = score_cost, label = ISO3)) +
  geom_point(aes(size = score_marketability, color = score_need)) +
  geom_text(size=3) +
  xlim(0,1) +
  ylim(0,1) +
  theme_bw() 

# "final" biplot

## with need NAs as different shape
data_complete <- data %>% filter(!is.na(score_regs) & !is.na(score_marketability) & !is.na(score_need))
data_incomplete <- data %>% filter(!is.na(score_regs) & !is.na(score_marketability) & is.na(score_need))
data_all <- rbind(data_complete, data_incomplete)
  
annotations <- data.frame(
  xpos = c(-Inf,-Inf,Inf,Inf),
  ypos =  c(-Inf, Inf,-Inf,Inf),
  annotateText = c("Bottom Left (h0,v0)","Top Left (h0,v1)"
                   ,"Bottom Right h1,v0","Top Right h1,v1"),
  hjustvar = c(0,0,1,1) ,
  vjustvar = c(0,1,0,1)) #<- adjust

ggplot() +
  geom_point(data = data_complete, shape = 16, aes(x = score_regs, y = score_marketability, size = score_need, color = score_need)) +
  geom_point(data = data_incomplete, shape = 1, aes(x = score_regs, y = score_marketability)) +
  # geom_text_repel(data = data_all, size = 3, force = 10, aes(x = score_regs, y = score_marketability, label = ISO3)) +
  # geom_text(data=annotations,aes(x=xpos,y=ypos,hjust=hjustvar,vjust=vjustvar,label=annotateText)) +
  xlim(0,1) +
  ylim(0,1) +
  scale_size_continuous(limits = c(0, 1), breaks=seq(0, 1, by = 0.25), name = "Social need") +
  scale_color_continuous(limits = c(0, 1), breaks=seq(0, 1, by = 0.25), name = "Social need") +
  guides(color= guide_legend(), size=guide_legend()) +
  geom_hline(yintercept = 0.5) +
  geom_vline(xintercept = 0.5) +
  labs(x = "Regulatory strength", y = "Marketability") +
  theme_minimal() 
ggsave(here("img", "biplot_blank.png"), width = 6, height = 5)






