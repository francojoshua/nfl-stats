pbp <- load_pbp(2021)

pbp |>
  filter((extra_point_attempt == 1 | field_goal_attempt == 1)) |>
  filter(week %in% 1:9) |>
  filter(season == 2025) |>
  group_by(season, team = posteam) |>
  summarize(
    attempts = n(),
    made = sum(extra_point_result == 'good' | field_goal_result == 'made', na.rm = TRUE),
    blocked = sum(extra_point_result == 'blocked' | field_goal_result == 'blocked', na.rm = TRUE),
    ratio = made / attempts
  ) |>
  arrange(ratio) |>
  print(n = 32)
