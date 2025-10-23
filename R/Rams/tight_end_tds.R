pbp <- load_pbp(1999:2025)

players <- load_players() |>
  select(gsis_id, position = pff_position)

pbp |>
  filter(down %in% 1:4 & !is.na(epa))  |>
  filter(touchdown == 1 & td_team == posteam & complete_pass == 1) |>
  left_join(players, by = c('receiver_player_id' = 'gsis_id')) |>
  select(receiver_player_name, position, yards_gained, air_yards, posteam, season, week) |>
  filter(position == 'TE' & posteam == 'LA') |>
  arrange(season) |>
  print(n=100)