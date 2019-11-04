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
  ggpairs()

# Plot the data
data %>% 
  mutate(score_econ = ifelse(is.na(score_econ), 0, score_econ)) %>%
  mutate(score_govt_breaks = case_when(score_govt <= quantile(score_govt, 1/3, na.rm = T) ~ "Low",
                                       score_govt >= quantile(score_govt, 2/3, na.rm = T) ~ "High",
                                       between(score_govt,
                                               quantile(score_govt, 1/3, na.rm = T),
                                               quantile(score_govt, 2/3, na.rm = T)) ~ "Med",
                                       T ~ "NA"),
         score_govt_breaks = fct_relevel(score_govt_breaks,
                                         "Low", "Med", "High", "NA")) %>% 
  ggplot(aes(x = score_cost, y = score_econ)) +
  geom_point(size = 2, shape = 21, fill = "gray") +
  geom_point(shape = 21, aes(size = score_nutrit, fill = score_govt_breaks)) +
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  geom_vline(xintercept = 0.5, linetype = "dashed") +
  geom_label_repel(aes(label = ISO3),
                   seed = 43,
                   point.padding = 0.7,
                   size = 2,
                   min.segment.length = 0) +
  startR::ggtheme_plot() +
  scale_fill_manual(values = c("red", "orange", "darkgreen", "gray")) +
  scale_size_continuous(breaks = c(0.3, 0.6, 1), range = c(1, 10)) +
  labs(x = "Cost score",
       y = "Market potential") +
  guides(size = guide_legend(title = "Nutrition\npotential"),
         fill = guide_legend(title = "Governance\n(terciles)", override.aes = list(size = 4))) +
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




















