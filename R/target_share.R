player_stats <- load_player_stats(summary_level = 'reg')

supporting_cast_epa <- player_stats |>
  group_by(team = recent_team) |>
  mutate(leader_id = player_id[which.max(target_share)]) |>
  filter(player_id != leader_id) |>
  ungroup() |>
  filter(position %in% c('WR', 'RB', 'TE')) |>
  group_by(team = recent_team) |>
  summarize(supporting_cast_epa = sum(receiving_epa, na.rm = TRUE)) |>
  arrange(-supporting_cast_epa)

supporting_cast_range <- range(supporting_cast_epa$supporting_cast_epa)

target_share_leaders <- player_stats |>
  select(player_id, team = recent_team, player_display_name, target_share, epa = receiving_epa) |>
  arrange(-target_share) |>
  mutate(rank = row_number()) |>
  filter(rank <= 10)
pbp <- load_pbp(2025)


important_data_all <- player_stats |> filter(position %in% c('WR', 'RB', 'TE')) |>
  slice_max(n = 100, order_by=targets) |>
  select(target_share, receiving_epa)
target_share_range <- range(important_data_all$target_share)
receiving_epa_range <- range(important_data_all$receiving_epa)

plot <- target_share_leaders |>
  left_join(supporting_cast_epa, by = c('team')) |>
  gt() |>
  nflplotR::gt_nfl_headshots(columns = player_id) |>
  nflplotR::gt_nfl_logos(columns = team) |>
  cols_label(
    player_id = '',
    player_display_name = 'Receiver',
    target_share = 'Target Share (%)',
    epa = 'Receiving EPA',
    supporting_cast_epa = 'Supporting Cast EPA'
  ) |>
  data_color(
    columns = c(target_share),
    method = "numeric",
    palette = "RdYlGn",
    domain = target_share_range
  ) |>
  data_color(
    columns = c(epa),
    method = "numeric",
    palette = "RdYlGn",
    domain = receiving_epa_range
  ) |>
  data_color(
    columns = c(supporting_cast_epa),
    method = "numeric",
    palette = "RdYlGn",
    domain = supporting_cast_range
  ) |>
  fmt_percent(target_share, decimal=1) |>
  fmt_number(columns = c(epa, supporting_cast_epa), decimal = 2) |>
  cols_move_to_start(rank) |>
  tab_footnote('¹ Supporting case EPA is the receiving EPA of the team without their team\'s largest target share leader') |>
  tab_source_note('Data: @nflfastR | By: Joshios') |>
  tab_header(
    title = '2025 NFL Target Share Leaders',
    subtitle = 'Including receiving EPA of the player and their supporting cast'
  ) |>
  cols_width(
    supporting_cast_epa ~ px(100),
    epa ~ px(100),
    target_share ~ px(100)
  ) |>
  gtExtras::gt_theme_nytimes() |>
  tab_style(
    locations = cells_body(columns = rank),
    style = cell_text(weight = 'bold')
  )


gtsave(plot, 'target_share.png')
