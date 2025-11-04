schedule <- load_schedules(2017:2025) |>
  nflreadr::clean_homeaway()


point_diffy <- schedule |> 
  filter(week %in% 1:9) |>
  group_by(team, season) |>
  summarize(
    point_differential = sum(team_score - opponent_score, na.rm = TRUE),
    wins = sum(team_score > opponent_score),
    ties = sum(team_score == opponent_score),
    losses = sum(team_score < opponent_score),
    record = paste0(wins, '-', losses),
    ratio = (wins + ties) / (wins + ties + losses)
  ) |>
  filter(team == "LA") |>
  arrange(-point_differential) |>
  ungroup() |>
  select(-wins, -ties, -losses)
 
point_diffy |>
  gt() |>
  tab_header(
    title = 'Point differential in the Sean McVay era',
    subtitle = 'Specifically through the first 9 weeks of the season'
  ) |>
  cols_label(
    point_differential='Point Differential'
  ) |>
  data_color(
    columns = c(point_differential),
    method = "numeric",
    palette = "RdYlGn"
  ) |>
  data_color(
    columns = c(ratio),
    method = "numeric",
    palette = "RdYlGn",
    target_columns = c(record)
  ) |>
  nflplotR::gt_nfl_logos(columns = team) |>
  cols_hide(columns = ratio) |>
  cols_width(
    point_differential ~ px(100)
  ) |>
  tab_source_note("Data: @nflfastR | Chart: Joshios") |>
  gtExtras::gt_theme_nytimes() |>
  gtsave('diffy.png')

