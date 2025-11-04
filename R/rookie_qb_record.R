schedules <- load_schedules(2016:2025) |>
  clean_homeaway(invert = c('result')) |>
  filter(!is.na(result))

players <- load_players()


draft_years <- players |>
  filter(position == 'QB') |>
  filter(!is.na(draft_year)) |>
  select(id = gsis_id, draft_year) 

schedules |> 
  select(season, team_coach, opponent_qb_id, opponent_qb_name, result) |>
  left_join(draft_years, by = c('opponent_qb_id' = 'id')) |>
  mutate(rookie = season == draft_year) |>
  filter(rookie) |>
  group_by(team_coach) |>
  summarize(
    record = mean(result > 0),
    games = n()
  ) |>
  filter(games >= 5) |>
  arrange(-record)
