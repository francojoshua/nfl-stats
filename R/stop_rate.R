pbp <- load_pbp(2024:2025)

points <- load_schedules(2024:2025) |>
  nflreadr::clean_homeaway() |>
  group_by(team, season) |>
  summarize(
    points_allowed = sum(opponent_score, na.rm = TRUE)
  ) |>
  arrange(points_allowed)

data <- pbp |>
  filter(down %in% 1:4) |>
  group_by(season, game_id, drive, team = defteam) |>
  summarize(
    score = any(series_result == 'Touchdown' | series_result == 'Field goal'),
    kneel = any(series_result == 'QB kneel'),
    zero_play = any(drive_play_count == 0)
  ) |>
  ungroup() |>
  group_by(season, team) |>
  summarize(
    drives = n() - sum(kneel) - sum(zero_play),
    scores = sum(score),
    stop_rate = mean(!score),
  ) |>
  ungroup() |>
  left_join(points, by = c('team', 'season')) |>
  arrange(-stop_rate) |>
  group_by(season) |>
  mutate(pts_per_drive = points_allowed / drives, rank = row_number()) |>
  select(-drives, -scores, -points_allowed)

data |> ungroup() |> pivot_wider(names_from = season, values_from = c(stop_rate, pts_per_drive, rank)) |>
  mutate(diff = rank_2024-rank_2025, combined = rank_2024+rank_2025) |>
  arrange(-combined)

stop_rate_range <- range(
  c(data$stop_rate),
  na.rm = TRUE
)

pts_drive_range <- range(
  c(data$pts_per_drive),
  na.rm = TRUE
)


data |> print(n = 32)

left <- data |> filter(rank <= 16)
right <- data |> filter(rank > 16)
left <- left |> rename_with(~ paste0(.x, "_L"))
right <- right |> rename_with(~ paste0(.x, "_R"))


table_data <- bind_cols(left, right)
table_data |>
  gt() |>
  fmt_percent(columns = c(stop_rate_L, stop_rate_R), decimals = 1) |>
  nflplotR::gt_nfl_logos(columns = c(team_L, team_R)) |>
  cols_move_to_start(columns=rank_L) |>
  cols_move(columns=rank_R, after=pts_per_drive_L) |>
  cols_label(
    stop_rate_L = 'Stop Rate',
    stop_rate_R = 'Stop Rate',
    rank_L = 'Rank',
    rank_R = 'Rank',
    team_L = 'Team',
    team_R = 'Team',
    pts_per_drive_L = 'Pts/Drive',
    pts_per_drive_R = 'Pts/Drive'
  ) |>
  gtExtras::gt_theme_nytimes() |>
  fmt_number(columns = c(pts_per_drive_L, pts_per_drive_R), decimals = 2) |>
  gt::tab_style(
    locations = cells_body(
      columns = c(rank_L, rank_R)
    ),
    style = cell_text(weight = 'bold')
  ) |>
  data_color(
    columns = c(stop_rate_L, stop_rate_R),
    method = "numeric",
    palette = "RdYlGn",
    domain = stop_rate_range
  ) |>
  data_color(
    columns = c(pts_per_drive_L, pts_per_drive_R),
    method = "numeric",
    palette = "RdYlGn",
    domain = pts_drive_range,
    reverse = TRUE
  ) |>
  gtExtras::gt_add_divider(columns = c(pts_per_drive_L), style = 'dashed') |>
  tab_source_note("Data: @nflfastR | Chart: Joshios") |>
  tab_header(
    title = '2025 NFL Stop Rate',
    subtitle = 'Stop rate is the % of drives that result in no score, excluding QB kneels'
  ) |>
  tab_options(
    data_row.padding = px(2)
  )
