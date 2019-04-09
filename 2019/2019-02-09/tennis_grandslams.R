library(tidyverse)

# read in data
player_dob <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-09/player_dob.csv")
grand_slams <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-09/grand_slams.csv")
grand_slam_timeline <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-04-09/grand_slam_timeline.csv")

# get age
age_slams_comb <- left_join(grand_slams, player_dob, by = c("name")) %>%
  mutate(age = tournament_date - date_of_birth) %>%
  group_by(name, age, gender) %>%
  summarize(counts = n()) %>%
  group_by(name) %>%
  mutate(total_wins = cumsum(counts)) %>%
  arrange(desc(total_wins)) %>%
  ungroup() %>%
  mutate(age = age / 365)

# find top 5 players
top_players <- age_slams_comb %>%
  group_by(name) %>%
  top_n(1, total_wins) %>%
  ungroup() %>%
  top_n(5, total_wins) %>%
  pull(name)

# create plot dataframe
plot_data <- age_slams_comb %>%
  ungroup() %>%
  mutate(
    colour = case_when(
      name == "Serena Williams" ~ "#003399",
      name == "Steffi Graf" ~ "#FF2B4F",
      name == "Roger Federer" ~ "#fcab27",
      name == "Chris Evert" ~ "#3686d3",
      name == "Martina Navratilova" ~ "#88398a",
      T ~ "gray80"),
    name = fct_reorder(name, total_wins)
    ) %>%
  mutate(hj = if_else(name == "Chris Evert", 1, 0))

# plot - a lot of borrowing from John Burn-Murdoch
# https://gist.github.com/johnburnmurdoch/bd20db77b2582031604ccd1bdc4be582

(plot_slams <- ggplot(
  plot_data,
  aes(age, total_wins,
    group = name, col = colour, fill = colour,
    alpha = name %in% top_players)) +
  theme_minimal() +
  geom_step(aes(size = name %in% top_players)) +
  geom_point(data = . %>%
    group_by(name) %>%
    top_n(1, total_wins), shape = 21, aes(col = colour), size = 2.5, stroke = 1) +
  geom_text(
    data = . %>%
      group_by(name) %>%
      top_n(1, total_wins) %>%
      filter(name %in% top_players) %>%
      mutate(
        first_initial = str_sub(name, 1, 1),
        last_name = gsub(".+\\s", "", name),
        short_name_wins = paste0("  ", first_initial, ". ", last_name, ":", total_wins, "  ")),
    aes(label = short_name_wins, hjust = hj), family = "Roboto Mono Medium") +
  scale_color_identity() +
  scale_fill_identity() +
  scale_alpha_manual(values = c(0.7, 1), guide = F) +
  scale_size_manual(values = c(0.5, 0.8), guide = F) +
  scale_x_continuous(limits = c(15, 40), breaks = seq(15, 35, 5), expand = c(0, 0)) +
  scale_y_continuous(position = "right", expand = expand_scale(add = c(0, 5))) +
  tomtom::theme_tom() +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "gray85", size = 0.3),
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_line(color = "#212121", size = 0.3),
    axis.ticks.length = unit(0.2, "cm"),
    axis.line.x = element_line(size = 0.3, color = "#212121"),
    axis.text.y.right = element_text(hjust = 1),
    axis.title.y = element_blank(),
    plot.caption = element_text(hjust = 0, face = "bold"),
    text = element_text(family = "Roboto Mono")) +
  labs(
    x = "\nAge",
    y = "",
    title = "Serena owns the most Grand Slam wins, but was less efficient than Graf",
    subtitle = "Cumulative Open Era Grand Slams won, by age",
    caption = "\nSource: Wikipedia | Graphic: Thomas Mock / @thomas_mock")
  )

ggsave("top_slams.png", width = 14, height = 8, units = "in")

colorblindr::cvd_grid(plot_slams)
