schedule <- load_schedules(2017:2025) |>
  nflreadr::clean_homeaway()

point_diffy <- schedule |> 
  filter(week %in% 1:6) |>
  group_by(team, season) |>
  summarize(
    point_differential = sum(team_score - opponent_score),
    wins = sum(team_score > opponent_score),
    ties = sum(team_score == opponent_score),
    losses = sum(team_score < opponent_score),
    record = paste0(wins, '-', losses)
  ) |>
  arrange(season) |>
  filter(team == 'LA') |>
  ungroup() |>
  select(-wins, -ties, -losses)

point_diffy |>
  gt() |>
  tab_header(
    title = 'Point differential in the Sean McVay era',
    subtitle = 'Specifically through the first 6 weeks of the season'
  ) |>
  cols_label(
    point_differential='Point Differential'
  ) |>
  nflplotR::gt_nfl_logos(columns = team) |>
  cols_width(
    point_differential ~ px(100)
  ) |>
  gtExtras::gt_theme_nytimes()
