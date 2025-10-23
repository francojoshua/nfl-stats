pbp <- load_pbp(2025)
roster <- load_rosters(2025) |>
  filter(position %in% c("RB", "QB", "WR", "TE")) |>
  group_by(gsis_id) |>
  reframe(
    position = first(na.omit(position))
  )

roster

data <- pbp |>
  filter(down %in% 1:4 & !is.na(receiver_player_id)) |>
  left_join(roster, by = c('receiver_player_id' = 'gsis_id')) |>
  filter(!is.na(position)) |>
  group_by(game_id, team = posteam, position) |>
  summarize(targets = n(), opponent = first(defteam), .groups = 'drop') |>
  group_by(team, game_id) |>
  mutate(
    targets_sum = sum(targets),
    targets_pct = (targets / targets_sum) * 100
  ) |>
  ungroup() |>
  select(-targets, -targets_sum) |>
  pivot_wider(
    id_cols = c(game_id, team, opponent),
    names_from = position,
    values_from = targets_pct,
    values_fill = 0
  ) |>
  filter(team == 'LA') |>
  select(-QB)

data
data |>
  gt() |>
  gt_plt_bar_pct(
    column = "WR",
    scaled = TRUE,
    fill = 'dodgerblue',
    labels = TRUE
  ) |>
  gt_plt_bar_pct(
    column = "RB",
    scaled = TRUE,
    fill = 'firebrick',
    labels = TRUE
  ) |>
  gt_plt_bar_pct(
    column = "TE",
    fill = 'seagreen',
    scaled = TRUE,
    labels = TRUE
  ) |>
  gtExtras::gt_theme_nytimes() |>
  gt_nfl_logos(columns = c(team, opponent)) |>
  cols_move_to_start(c('team', 'opponent', 'WR', 'TE', 'RB')) |>
  cols_hide(game_id) |>
  tab_source_note("Data: @nflfastR | By: Joshios") |>
  tab_header(
    title = '2025 Target Share Between Positions',
   subtitle = 'For the Los Angeles Rams offense specifically'
  ) |>
  gtsave('test.png')
