pbp <- load_pbp(2024) |>
  filter(down %in% 1:4 & !is.na(epa) & !is.na(passer_id)) |>
  filter(season_type =='REG')

participation <- load_participation(2024) |>
  select(game_id = nflverse_game_id, play_id, was_pressure)

pbp <- left_join(pbp, participation, by = c('game_id', 'play_id'))


combined <- pbp |>
  group_by(team = defteam) |>
  summarize(
    pressure_rate = mean(was_pressure),
    yards_game = sum(passing_yards, na.rm = TRUE) / n_distinct(game_id)
  ) |>
  arrange(yards_game)


x_min <- min(combined$pressure_rate, na.rm = TRUE)
x_max <- max(combined$pressure_rate, na.rm = TRUE)
y_min <- min(combined$yards_game, na.rm = TRUE)
y_max <- max(combined$yards_game, na.rm = TRUE)
x_range <- x_max - x_min
y_range <- y_max - y_min



plot <- ggplot2::ggplot(combined, aes(x = pressure_rate, y = yards_game)) +
  nflplotR::geom_mean_lines(aes(x0 = pressure_rate , y0 = yards_game)) +
  nflplotR::geom_nfl_logos(aes(team_abbr = team), width = 0.065, alpha = 0.7) +
  ggplot2::labs(
    x = "Pressure rate %",
    y = "Passing yards allowed / game",
    caption = "Data: @nflfastR | Joshios",
    title = "2024 Defensive Efficiency",
    subtitle = 'Measured by pass yards/game and pressure rate'
  ) +
  scale_y_reverse() +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    plot.title = ggplot2::element_text(face = "bold"),
    plot.title.position = "plot",
    plot.background = ggplot2::element_rect(fill = "#F0F0F0")
  )

plot

ggsave('pressure.png', plot = plot)
