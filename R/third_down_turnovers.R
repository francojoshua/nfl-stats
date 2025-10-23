pbp <- load_pbp(2025)


turnovers <- pbp |>
  filter(down %in% 1:4) |>
  group_by(
    team = defteam
  ) |>
  summarize(
    ints = sum(interception, na.rm = TRUE),
    ff = sum(fumble_lost, na.rm = TRUE),
    safety = sum(safety, na.rm = TRUE),
    turnovers = ints + ff + safety
  ) |>
  select(team, turnovers)

third_down_stop <- pbp |>
  filter(down == 3) |>
  group_by(team = defteam) |>
  summarize(
    stop_pct = 1 - mean(first_down, na.rm = TRUE)
  )

combined <- full_join(third_down_stop, turnovers)

x_min <- min(combined$stop_pct, na.rm = TRUE)
x_max <- max(combined$stop_pct, na.rm = TRUE)
y_min <- min(combined$turnovers, na.rm = TRUE)
y_max <- max(combined$turnovers, na.rm = TRUE)
x_range <- x_max - x_min
y_range <- y_max - y_min



plot <- ggplot2::ggplot(combined, aes(x = stop_pct, y = turnovers)) +
  nflplotR::geom_mean_lines(aes(x0 = stop_pct , y0 = turnovers)) +
  nflplotR::geom_nfl_logos(aes(team_abbr = team), width = 0.065, alpha = 0.7) +
  ggplot2::labs(
    x = "3rd down stop %",
    y = "Takeaways",
    caption = "Data: @nflfastR | Joshios",
    title = "2025 Defensive Efficiency",
    subtitle = 'For a defense, the % of 3rd downs not resulting in a first down and turnovers caused'
  ) +
  ggplot2::theme_minimal() +
  annotate("text",
           x = x_max - x_range * 0.15,
           y = y_max - y_range * 0.15,
           label = "Win stops + win turnovers",
           fontface = "bold", color = "darkgreen") +
  annotate("text",
           x = x_min + x_range * 0.15,
           y = y_max - y_range * 0.15,
           label = "Turnovers but lack stops",
           fontface = "bold", color = "orange") +
  annotate("text",
           x = x_min + x_range * 0.15,
           y = y_min + y_range * 0.15,
           label = "Lose both battles",
           fontface = "bold", color = "red") +
  annotate("text",
           x = x_max - x_range * 0.15,
           y = y_min + y_range * 0.15,
           label = "Win stops but few turnovers",
           fontface = "bold", color = "blue") +
  ggplot2::theme(
    plot.title = ggplot2::element_text(face = "bold"),
    plot.title.position = "plot",
    plot.background = ggplot2::element_rect(fill = "#F0F0F0")
  )

ggsave('defense.png', plot = plot)
