nflreadr::clear_cache()
pbp <- load_pbp(2025)

data <- pbp |>
  filter(down %in% 1:4 & !is.na(epa) & (rush == 1 | pass == 1)) |>
  filter(!is.na(passer_player_id)) |>
  group_by(game_id, id = passer_player_id) |>
  reframe(
    name = first(passer_player_name),
    week = paste0('W', first(week)),
    team = first(posteam),
    plays = n(),
    epa = mean(epa)
  ) |>
  group_by(game_id) |>
  slice_max(plays, n = 2, with_ties = FALSE) |>
  filter(epa > 0.1) |>
  filter(n() == 2) |>
  mutate(
    epa_combined = sum(epa),
    plays = paste0('n = ', plays)
  ) |>
  arrange(-epa) |>
  mutate(rank = row_number()) |>
  ungroup() |>
  pivot_wider(
    id_cols = c(game_id, epa_combined, week),
    names_from = rank,
    values_from = c(id, name, team, plays, epa),
    names_glue = "{.value}_{rank}"
  ) |>
  ungroup() |>
  arrange(-epa_combined)


combined_range <- range(data$epa_combined)

epa <- pbp |>
  filter(down %in% 1:4 & !is.na(epa) & (rush == 1 | pass == 1) & !is.na(passer_player_id)) |>
  group_by(game_id, id = passer_player_id) |>
  reframe(
    epa = mean(epa),
    n = n()
  ) |>
  filter(n >= 15)
epa_range <- range(epa$epa)

epa_range

plot <- data |>
  mutate(rank = row_number()) |>
  filter(rank <= 10) |>
  gt() |>
  gt_nfl_headshots(columns = starts_with('id')) |>
  gt_nfl_logos(columns = starts_with('team')) |>
  fmt_number(columns = starts_with('epa'), decimals = 2) |>
  gt_merge_stack(col1 = name_1, col2 = plays_1) |>
  gt_merge_stack(col1 = name_2, col2 = plays_2) |>
  cols_hide(game_id) |>
  cols_move_to_start(columns = c(rank, week, id_1, name_1, team_1, epa_1, id_2, name_2, team_2, epa_2, epa_combined)) |>
  cols_move_to_end(epa_combined) |>
  cols_label(
    starts_with("id") ~ "",
    starts_with("team") ~ "",
    starts_with("name") ~ "Passer",
    starts_with("epa") ~ "EPA/Play",
    epa_combined = 'Combined EPA'
  ) |>
  gt_add_divider(columns = c(epa_1)) |>
  gt_add_divider(columns = c(epa_2)) |>
  gt_add_divider(columns = c(week), style = 'dashed', include_labels = FALSE) |>
  gtExtras::gt_theme_nytimes() |>
  tab_style(
    locations = cells_body(columns = c(rank)),
    style = cell_text(weight = 'bold')
  ) |>
  data_color(
    palette = "RdYlGn",
    columns = c(epa_1, epa_2),
    domain = epa_range
  ) |>
  tab_header(
    "2025 NFL Top Games by Combined Quarterback EPA",
    subtitle = "Ranked by the sum of the starting QBs' EPA, where both QBs had an EPA/play over 0.1"
  ) |>
  tab_source_note("Data: @nflfastR | Chart: Joshios") |>
  tab_options(
    data_row.padding = px(2.5)
  )

plot
gtsave(plot, 'combined.png')
