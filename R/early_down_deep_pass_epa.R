pbp21_25 <- load_pbp(2021:2025)

teams <- load_teams() |> select(team = team_abbr, team_color, team_color2)

rates <- pbp21_25 |>
  filter(!is.na(epa) & pass == 1 & week %in% 1:7) |>
  filter(down %in% 1:2 & pass_attempt & air_yards >= 20) |>
  group_by(season, team = posteam) |>
  summarize(
    epa = mean(epa)
  ) |>
  left_join(teams, by = c('team'))

rates
rates |>
  pivot_wider(
    names_from = season,
    names_prefix = 'epa',
    values_from = epa
  ) |>
  mutate(diff = epa2025 - epa2024) |>
  arrange(-diff) |>
  print(n = 32)

teams_to_plot <- c("LA")

plot <- ggplot(rates, aes(x = season, y = epa, group = team)) +
  geom_line(
    size = 1.2,
    aes(group = team, color = team_color2),
    data = filter(rates, team %in% teams_to_plot)
  ) +
  geom_point(size = 2, aes(color = team_color), alpha = 0.4) +
  geom_nfl_logos(aes(team_abbr = team), width = 0.075, data = filter(rates, team %in% teams_to_plot)) +
  labs(
    title = "EPA/Play on Early Down Deep Passes (20+ Air Yards, Downs 1-2)",
    subtitle = 'From 2021 - 2025, Weeks 1 through 7',
    x = "Year",
    y = "EPA/Play",
    caption = "Data: @nflfastR | By: Joshios"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  ) +
  scale_color_identity() + 
  scale_y_continuous(breaks = c(-0.5, 0, 0.5, 1, 1.5)) + 
  theme(
    plot.title = element_text(face = "bold", size = 16),
    axis.title.x = element_text(face = "bold", size = 14),
    axis.title.y = element_text(face = "bold", size = 14)
  )

ggsave(plot, filename='epa.png')
