schedules <- load_schedules(1980:2025) |>
  clean_homeaway(invert = 'result')


info <- load_teams() |>
  select(team_abbr, team_conf, team_division)

schedules |> glimpse()

schedules |>
  mutate(
    team = recode(team,
                  "STL" = "LA",
                  "SD"  = "LAC",
                  "OAK" = "LV"
    )
  ) |>
  filter(season == 2025) |>
  left_join(info, by =c('team' = 'team_abbr')) |>
  left_join(info, by = c('opponent' = 'team_abbr')) |>
  rename(opponent_conf = team_conf.y, opponent_division = team_division.y) |>
  filter(week %in% 1:9) |>
  rename(team_conf = team_conf.x, team_division = team_division.x) |>
  group_by(season, team_division) |>
  summarize(
    wins = sum(result > 0, na.rm = TRUE),
    diff_conf_wins = sum(result > 0 & opponent_conf != team_conf, na.rm = TRUE),
    afc_loss = sum(result < 0 & opponent_conf != team_conf, na.rm = TRUE),
    other_wins = sum(result > 0 & opponent_division != team_division, na.rm = TRUE)
  ) |>
  arrange(-wins) |>
  print(n = 100)
