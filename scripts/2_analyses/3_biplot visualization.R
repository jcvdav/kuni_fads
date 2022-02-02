
# Load packages

library(startR)
library(here)
library(scales)
library(janitor)

# Load data

scaled_data <- read.csv(here("data", "data_scaled.csv"),
                        stringsAsFactors = F) %>%
  select(alpha_3, score_regs = score_govt, score_wgi, score_need, score_marketability, n_fads, n_private, fads_per_totvessel) %>%
  mutate(p_private = n_private/n_fads)

cost_data <- read.csv(here("data", "country_level_cost_summary_statistics.csv"),
                      stringsAsFactors = F) %>% 
  mutate(score_cost = rescale(mean, to = c(0, 1))) %>% 
  select(ISO3, score_cost)

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

data_complete <- data %>% filter(!is.na(score_regs) & !is.na(score_marketability) & !is.na(score_need))
data_incomplete <- data %>% filter(!is.na(score_regs) & !is.na(score_marketability) & is.na(score_need))

# Plot data

ggplot() +
  geom_point(data = data_complete, shape = 16, aes(x = score_regs, y = score_marketability, size = score_need, color = score_need)) +
  geom_point(data = data_incomplete, shape = 1, aes(x = score_regs, y = score_marketability)) +
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