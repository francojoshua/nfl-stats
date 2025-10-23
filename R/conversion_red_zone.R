pbp <- load_pbp(2025)


redzone <- pbp |>
  filter(down %in% 1:4 & !is.na(epa)) |>
  filter(yardline_100 <= 20) |>
  group_by(team = posteam) |>
  reframe(
    n = n(),
    conversion_rate = mean(first_down == 1, na.rm = TRUE),
    average_down_on_conversion = mean(down[first_down == 1], na.rm = TRUE)
  ) |>
  arrange(-conversion_rate) |>
  mutate(rank = row_number())

redzone |> print(n=32)
  
plot <- ggplot(redzone, aes(x = conversion_rate, y = average_down_on_conversion)) +
  geom_mean_lines(aes(x0 = conversion_rate, y0 = average_down_on_conversion)) +
  geom_nfl_logos(aes(team_abbr = team), width = 0.065, alpha = 0.7) +
  labs(
    x = "Conversion Rate (First Down or Touchdown)",
    y = "Average Down On Converted Red Zone Plays",
    caption = "Data: @nflfastR | Chart: Joshios | Inspiration: @CFBNumbers",
    subtitle = 'The X axis is the percentage of all red zone plays resulting in a first down or touchdown.\nThe Y axis is the average down of the converted red zone plays.',
    title = '2025 NFL Week 1-6 Red Zone Efficiency'
  ) +
  ggplot2::scale_y_reverse(breaks = breaks_pretty(n = 6)) +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    plot.title = ggplot2::element_text(face = "bold")
  ) +
  scale_x_continuous(
    labels = scales::percent_format(accuracy = 1)
  )

plot

ggsave('effiency.png')

