pbp <- load_pbp(2025)

explosive2025 <- pbp |>
  filter(down %in% 1:4 & !is.na(epa) & (rush == 1 | pass == 1)) |>
  group_by(team = posteam) |>
  reframe(
    rush_plays = sum(rushing_yards >= 10, na.rm = TRUE),
    pass_plays = sum(receiving_yards >= 20, na.rm = TRUE),
    total_plays = (rush_plays + pass_plays) / n_distinct(game_id)
  ) |>
  arrange(-total_plays) |>
  mutate(rank = row_number())

explosive2024 <- load_pbp(2024) |>
  filter(down %in% 1:4 & !is.na(epa) & (rush == 1 | pass == 1)) |>
  group_by(team = posteam) |>
  reframe(
    rush_plays = sum(rushing_yards >= 10, na.rm = TRUE),
    pass_plays = sum(receiving_yards >= 20, na.rm = TRUE),
    total_plays = (rush_plays + pass_plays) / n_distinct(game_id)
  ) |>
  arrange(-total_plays) |>
  mutate(rank = row_number())


explosive2024 <- rename_with(explosive2024, ~ paste0(.x, '_2024'))

combined <- left_join(explosive2025, explosive2024, by = c('team' = 'team_2024'))

combined |>
  mutate(diff = rank_2024 - rank) |>
  arrange(diff)
