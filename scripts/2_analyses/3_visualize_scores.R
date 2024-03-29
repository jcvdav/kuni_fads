##########################################
## Create figures that relate all scores
##########################################

# Load packages
library(startR)
library(here)
library(GGally)
library(ggrepel)
library(scales)
library(tidyverse)

# Load data
## Scaled data
scaled_data <- read.csv(here("data", "data_scaled.csv"),
                        stringsAsFactors = F) %>% 
  select(alpha_3, contains("score"))

## Cost data
cost_data <- read.csv(here("data", "country_level_cost_summary_statistics.csv"),
                      stringsAsFactors = F) %>% 
  mutate(score_cost = rescale(mean, to = c(0, 1))) %>% 
  select(ISO3, score_cost)

## Combine data
data <- cost_data %>% 
  left_join(scaled_data, by = c("ISO3" = "alpha_3"))

# Create ggpairs
data %>%
  select(contains("score")) %>%
  ggpairs() +
  ggtheme_plot()

ggsave(filename = here("img", "score_pair_plot.png"),
       width = 7,
       height = 4)

# Plot the data
data %>% 
  mutate(score_marketability = ifelse(is.na(score_marketability), 0, score_marketability)) %>%
  mutate(score_govt_breaks = case_when(score_govt <= quantile(score_govt, 1/3, na.rm = T) ~ "Low",
                                       score_govt >= quantile(score_govt, 2/3, na.rm = T) ~ "High",
                                       between(score_govt,
                                               quantile(score_govt, 1/3, na.rm = T),
                                               quantile(score_govt, 2/3, na.rm = T)) ~ "Med",
                                       T ~ "NA"),
         score_govt_breaks = fct_relevel(score_govt_breaks,
                                         "Low", "Med", "High", "NA")) %>% 
  ggplot(aes(x = score_cost, y = score_marketability)) +
  geom_point(size = 1, shape = 21, fill = "gray") +
  geom_point(shape = 21, aes(size = score_need, fill = score_govt_breaks)) +
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  geom_vline(xintercept = 0.5, linetype = "dashed") +
  geom_label_repel(aes(label = ISO3),
                   seed = 43,
                   point.padding = 0.7,
                   size = 2,
                   min.segment.length = 0) +
  startR::ggtheme_plot() +
  theme_bw() +
  scale_fill_manual(values = c("red", "orange", "darkgreen", "gray")) +
  scale_size_continuous(breaks = c(0.3, 0.6, 1), range = c(1, 10)) +
  labs(x = "Biophysical cost",
       y = "Marketability of catch") +
  guides(size = guide_legend(title = "Social\npotential"),
         fill = guide_legend(title = "Governance\ncapacity\n(terciles)", override.aes = list(size = 4))) +
  scale_x_continuous(limits = c(0, 1)) +
  scale_y_continuous(limits = c(0, 1)) +
  coord_equal()

# Export the figure
ggsave(plot = last_plot(),
       filename = here("img", "score_figure.pdf"),
       width = 6,
       height = 5.5)

# Export the figure
ggsave(plot = last_plot(),
       filename = here("img", "score_figure.png"),
       width = 6,
       height = 5.5)




















