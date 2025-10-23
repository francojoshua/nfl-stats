pbp <- load_pbp(2025)


rank2025 <- pbp |>
  filter(down %in% 1:4 & !is.na(epa) & (rush == 1 | pass == 1)) |>
  group_by(team = posteam) |>
  summarize(
    ten_yard_plays = sum(yards_gained >= 10, na.rm = TRUE),
    plays = n(),
    pct = ten_yard_plays / plays
  ) |>
  arrange(-pct) |>
  mutate(rank = row_number()) |>
  print(n=32)

pbp24 <- load_pbp(2024)

rank2024 <- pbp24 |>
  filter(down %in% 1:4 & !is.na(epa) & (rush == 1 | pass == 1)) |>
  group_by(team = posteam) |>
  summarize(
    ten_yard_plays = sum(yards_gained >= 10, na.rm = TRUE),
    plays = n(),
    pct = ten_yard_plays / plays
  ) |>
  arrange(-pct) |>
  mutate(rank2 = row_number()) |>
  print(n=32)


full_join(rank2025, rank2024, by = c('team')) |>
  mutate(diff = rank2-rank) |>
  arrange(diff)
