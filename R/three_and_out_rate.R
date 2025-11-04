pbp <- load_pbp(2025)


pbp |>
  filter(down == 3) |>
  group_by(team = posteam) |>
  summarize(
    n = n(),
    first_down = sum(first_down, na.rm = TRUE),
    efficiency = first_down / n,
    average_yard = mean(ydstogo, na.rm = TRUE),
    average_gain = mean(yards_gained, na.rm = TRUE)
  ) |>
  arrange(efficiency) |>
  print(n = 32)


off <- pbp |>
  filter(down %in% 1:4) |>
  group_by(team = posteam, game_id, drive) |>
  summarize(
    first_downs = sum(first_down, na.rm = TRUE),
    series_result = first(series_result),
    groups = '.drop'
  ) |>
  group_by(team) |>
  summarize(
    off_drives = n(),
    off_three_and_out_rate = 
      sum(first_downs == 0 & (series_result == 'Punt' | series_result == 'Turnover' | series_result == 'Turnover on downs'))/off_drives
  ) |>
  arrange(-off_three_and_out_rate)

def <- pbp |>
  filter(down %in% 1:4) |>
  group_by(team = defteam, game_id, drive) |>
  summarize(
    first_downs = sum(first_down, na.rm = TRUE),
    series_result = first(series_result),
    groups = '.drop'
  ) |>
  group_by(team) |>
  summarize(
    def_drives = n(),
    def_three_and_out_rate = 
      sum(first_downs == 0 & (series_result == 'Punt' | series_result == 'Turnover' | series_result == 'Turnover on downs'))/def_drives
  ) |>
  arrange(-def_three_and_out_rate)


pbp |>
  filter(down %in% 1:4 & defteam == 'LA') |>
  filter(series_result == 'Punt' | series_result == 'Turnover' | series_result == 'Turnover on downs') |>
  group_by(game_id, drive) |>
  summarize(
    result = first(drive_ended_with_score),
    plays = n(),
    yards_gained = sum(yards_gained, na.rm = TRUE),
    first_downs = sum(first_down, na.rm = TRUE),
    test = first(series_result)
  ) |>
  filter(first_downs == 0) |>
  print(n = 44)

pbp |>
  filter(game_id == '2025_04_IND_LA' & drive == 19) |>
  select(desc, ydstogo, down)


combined <- full_join(off, def, by=c('team'))

ggplot2::ggplot(combined, aes(x = off_three_and_out_rate, y = def_three_and_out_rate)) +
  nflplotR::geom_mean_lines(aes(x0 = off_three_and_out_rate , y0 = def_three_and_out_rate)) +
  nflplotR::geom_nfl_logos(aes(team_abbr = team), width = 0.065, alpha = 0.7) +
  ggplot2::labs(
    x = "Offensive three and out rate",
    y = "Defensive three and out rate",
    caption = "Data: @nflfastR | Joshios",
    subtitle = "How often a team avoids or forces three-and-outs (a drive with no first down) on both sides of the ball",
    title = "2025 Team Three-and-Out Performance (Offense vs. Defense) through Week 8"
  ) +
  ggplot2::theme_minimal() +
  ggplot2::scale_x_reverse(labels = scales::percent) +
  ggplot2::scale_y_continuous(labels = scales::percent) +
  # quadrant text labels
  annotate("text", x = max(combined$off_three_and_out_rate, na.rm=TRUE) * 0.9,
           y = max(combined$def_three_and_out_rate, na.rm=TRUE) * 0.9,
           label = "Defense shines", fontface="bold", color="orange") +
  annotate("text", x = min(combined$off_three_and_out_rate, na.rm=TRUE) * 1.1,
           y = max(combined$def_three_and_out_rate, na.rm=TRUE) * 0.8,
           label = "Strong both ways", fontface="bold", color="darkgreen") +
  annotate("text", x = min(combined$off_three_and_out_rate, na.rm=TRUE) * 1.2,
           y = min(combined$def_three_and_out_rate, na.rm=TRUE) * 1.1,
           label = "Offense shines", fontface="bold", color="red") +
  annotate("text", x = max(combined$off_three_and_out_rate, na.rm=TRUE) * 0.90,
           y = min(combined$def_three_and_out_rate, na.rm=TRUE) * 1.1,
           label = "Struggles both ways", fontface="bold", color="blue") +
  ggplot2::theme(
    plot.title = ggplot2::element_text(face = "bold"),
    plot.title.position = "plot",
    plot.background = ggplot2::element_rect(fill = "#F0F0F0")
  )
