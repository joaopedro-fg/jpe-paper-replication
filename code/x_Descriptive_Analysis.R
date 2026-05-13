library(tidyverse)
library(here)
fed_data <- readRDS(here("data", "fed_data.rds"))
income_data <- read.csv(here("raw_data", "Income  Data.csv"))
# First, we need to normalize types and names before merging both dfs
fed_data <- fed_data %>% mutate(year = as.integer(year))
income_data <- income_data %>% rename(year = Year, quarter = Quarter)
# We also need to pivot the data
income_data_wide <- income_data %>% 
                select(year, quarter, Group, Real.Factor.Income.Per.Unit) %>% 
                pivot_wider(names_from = "Group", values_from = "Real.Factor.Income.Per.Unit")
data <- fed_data %>%
        inner_join(income_data_wide, by = c("year", "quarter")) %>%
        mutate(log_top_10 = log(`Top 10%`),
                log_bottom_50 = log(`Bottom 50%`),
                log_top_1 = log(`Top 1%`),
                log_top_01 = log(`Top 0.1%`))
# Scatter plot
plot <- ggplot(data, aes(x = real_gdp)) +
  geom_point(aes(y = log_top_10,    color = "Top 10%"),    alpha = 0.7, size = 5) +
  geom_point(aes(y = log_bottom_50, color = "Bottom 50%"), alpha = 0.7, size = 5) +
  geom_point(aes(y = log_top_1, color = "Top 1%"), alpha = 0.7, size = 5) +
  geom_point(aes(y = log_top_01, color = "Top 0.1%"), alpha = 0.7, size = 5) +
  scale_color_manual(
    values = c("Top 10%" = "#E63946", "Bottom 50%" = "#457B9D", "Top 1%" = "#FFD580", "Top 0.1%" = "#90EE90")
  ) +
  labs(
    x     = "Real GDP",
    y     = "Log Income",
    color = "Group",
    title = "Income vs. Real GDP"
  ) +
  theme_minimal()
ggsave(here("plots", "scatter_plot_income.png"), plot, width = 15, height = 12)