pbp <- load_pbp(2015:2025)


pbp |>
  filter(kickoff_attempt == 1 & season %in% c(2024,2025)) |>
  group_by(season, team = defteam) |>
  summarize(
    epa = sum(epa),
    kickoff_inside_twenty = mean(kickoff_inside_twenty, na.rm = TRUE)
  ) |>
  arrange(-kickoff_inside_twenty) |>
  print(n=32)
