library(tidyverse)
library(readxl)

cost_df <- read_xlsx("us_avg_tuition.xlsx")

example_plot <- cost_df %>% 
    gather(year, tuition, `2004-05`:`2015-16`) %>% 
    filter(year == "2015-16") %>% 
    ggplot(aes(x = fct_reorder(State, tuition), y = tuition)) +
    geom_bar(stat = "identity") + 
    coord_flip() +
    labs(x = "State", y = "Tuition ($)",
         title = "College tuition costs in the USA (2015-16",
         caption = "\nDataSource: https://trends.collegeboard.org/ | Graphic: @thomas_mock")

example_plot

ggsave("example_plot.png", example_plot, height = 12, width = 6, units = "in", dpi = 600)
