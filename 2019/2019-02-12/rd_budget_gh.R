library(tidyverse)
library(ggrepel)
library(patchwork)
library(tomtom)

# Read in the data
df_spending <- read_csv("fed_r_d_spending.csv")
df_energy <- read_csv("energy_spending.csv")
df_climate <- read_csv("climate_spending.csv")


# scale data to billions
scaled_data <- df_spending %>% 
  filter(rd_budget != 0) %>% 
  mutate(decade = 10 * (year %/% 10),
         rd_budget = rd_budget/1e9,
         department = factor(department),
         department = fct_reorder(department, rd_budget, desc = FALSE))

# create the quant25/75 dataset
range_df <- scaled_data %>% 
  filter(rd_budget != 0) %>% 
  group_by(department) %>% 
  summarize(quant25 = quantile(rd_budget, 0.25),
            quant75 = quantile(rd_budget, 0.75)) %>% 
  right_join(scaled_data, by = c("department"))

# create plot
spending_dollars <- scaled_data %>% 
  ggplot(aes(x = year, y = rd_budget), color = "black") +
  geom_ribbon(data = range_df,
              aes(ymin = quant25, ymax = quant75),
              fill = "grey", alpha = 0.5) +
  geom_line() +
  geom_point(data = scaled_data %>%
               group_by(department) %>%
               filter(year %in% c(min(year), max(year)))) +
  geom_point(data = scaled_data %>%
               group_by(department) %>%
               filter(year == decade), color = "red") +
  facet_wrap(~fct_rev(department), ncol = 2, scales = "free_y") +
  tomtom::theme_tom() +
  scale_y_continuous(labels = scales::dollar_format()) +
  labs(x = "\nFiscal Year",
       y = "Inflation Adjusted Budget (Billions)\n",
       title = "Federal Research and Development spending is generally increasing",
       subtitle = "Grey ribbon = quantile-25 & quantile-75\nRed dot = decade") +
  theme(strip.text = element_text(face = "bold"))

ggsave("rd_dollars.png", spending_dollars, height = 16, width = 8, units = "in", dpi = "retina")

# Create the percent df and plot

df_percent <- df_spending %>% 
  filter(rd_budget != 0) %>% 
  group_by(year, department) %>% 
  summarize(percent_gdp = rd_budget/gdp,
            percent_budget = rd_budget/total_outlays) %>%
  ungroup() %>% 
  mutate(decade = 10 * (year %/% 10),
         department = factor(department),
         department = fct_reorder(department, percent_budget, desc = FALSE))

df_percent_range <- df_percent %>% 
  group_by(department) %>% 
  summarize(quant25 = quantile(percent_budget, 0.25),
            quant75 = quantile(percent_budget, 0.75)) %>% 
  right_join(df_percent, by = c("department"))

spending_percent <- df_percent %>% 
  ggplot(aes(x = year, y = percent_budget), color = "black") +
  geom_ribbon(data = df_percent_range,
              aes(ymin = quant25, ymax = quant75),
              fill = "grey", alpha = 0.5) +
  geom_line() +
  geom_point(data = df_percent %>%
               group_by(department) %>%
               filter(year %in% c(min(year), max(year)))) +
  geom_point(data = df_percent %>%
               group_by(department) %>%
               filter(year == decade), color = "red") +
  
  facet_wrap(~fct_rev(department), ncol = 2, scales = "free_y") +
  tomtom::theme_tom() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.01)) +
  labs(x = "\nFiscal Year",
       y = "Percent of Federal Budget",
       title = "... but Federal R&D spending as a percent of the budget is declining",
       subtitle = "Grey ribbon = quantile-25 & quantile-75\nRed dot = decade marker") +
  theme(strip.text = element_text(face = "bold"))

ggsave("rd_percent.png", spending_percent, height = 16, width = 8, units = "in", dpi = "retina")