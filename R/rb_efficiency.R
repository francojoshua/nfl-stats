pbp <- load_pbp(2025)
library(ggrepel)


data <- pbp |>
  filter(down %in% 1:4 & rush == 1) |>
  transmute(
    id = rusher_player_id,
    name = rusher_player_name,
    yards = ifelse(!is.na(yards_gained), yards_gained, 0)
  ) |>
  filter(!is.na(id)) |>
  group_by(id, name) |>
  summarize(
    plays = n(),
    ten_yard_plays = mean(yards > 10)
  ) |>
  arrange(-ten_yard_plays) |>
  ungroup() |>
  filter(plays > max(plays) * 0.50)
  
data

plot <- ggplot(data, aes(x = plays, y = ten_yard_plays, label = name)) +
  geom_label_repel() +
  geom_nfl_headshots(aes(player_gsis = id), width = 0.05, alpha = 0.8) +
  scale_y_continuous(labels = scales::percent) +
  geom_median_lines(aes(x0 = plays, y0 = ten_yard_plays)) +
  theme_minimal() +
  labs(
    x = "Designed runs",
    y = 'Explosive run rate (%)',
    title = 'Usage and efficiency of RBs through 2025 NFL Week 9',
    subtitle = 'An explosive run is categorized as a 10+ yard run',
    caption = 'Data: @nflfastR | Chart: Joshios'
  ) +
  theme(
    plot.title = ggplot2::element_text(face = 'bold')
  )

ggsave(plot=plot, 'test.png')
