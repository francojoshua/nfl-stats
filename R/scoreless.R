pbp |>
  filter(down %in% 1:4 & !is.na(epa)) |>
  filter(game_half != 'Overtime') |>
  group_by(game_id, qtr, team = defteam) |>
  summarize(
    n = n(),
    touchdown = sum(touchdown & td_team == posteam, na.rm = TRUE),
    fg = sum(field_goal_result == 'made', na.rm = TRUE)
  ) |>
  ungroup() |>
  group_by(team) |>
  summarize(
    n = n(),
    scoreless = sum(touchdown == 0 & fg == 0),
    ratio = scoreless / n
  ) |>
  arrange(-scoreless)
