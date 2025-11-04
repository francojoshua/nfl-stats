pbp <- load_pbp(2025)


qb_rank <- pbp |>
  filter(down %in% 1:4 & !is.na(epa) & (pass == 1 | rush == 1)) |>
  filter(vegas_wp > 0.05) |>
  group_by(game_id, week, team = posteam) |>
  summarize(
    qb_id = stat_mode(na.omit(passer_player_id)),
    qb_name = stat_mode(na.omit(passer_player_name)),
    qb_snaps = paste0('n = ', sum(pass_attempt & passer_player_id == qb_id, na.rm = TRUE)),
    qb_epa = mean(qb_epa[passer_player_id == qb_id], na.rm = TRUE)
  ) |>
  ungroup() |>
  group_by(week) |>
  arrange(-qb_epa) |>
  mutate(qb_rank = row_number()) |>
  filter(week == 9)

def_rank <- pbp |>
  filter(down %in% 1:4 & !is.na(epa) & (pass == 1 | rush == 1)) |>
  filter(vegas_wp > 0.05) |>
  group_by(game_id, week, team = defteam) |>
  summarize(
    def_epa = mean(epa)
  ) |>
  ungroup() |>
  group_by(week) |>
  arrange(def_epa) |>
  mutate(def_rank = row_number()) |>
  filter(week == 9)


def_rank

combined <- full_join(def_rank, qb_rank, by = c('team', 'week', 'game_id')) |>
  ungroup() |>
  mutate(diff = def_rank - qb_rank) |>
  arrange(diff) |>
  select(-def_epa, -qb_epa, -game_id, -week)

combined
left <- combined |> head(n = 13) |> rename_with(~ paste0(.x, "_L"))
right <- combined |> tail(n = 13) |> rename_with(~ paste0(.x, "_R"))


bind_cols(left, right)

bind_cols(left, right) |> gt() |>
  gt_nfl_headshots(starts_with('qb_id')) |>
  gt_nfl_logos(starts_with('team')) |>
  cols_move_to_start(c(qb_id_L, qb_name_L, team_L, qb_rank_L, def_rank_L, diff_L)) |>
  cols_move_to_end(c(qb_id_R, qb_name_R, team_R, qb_rank_R, def_rank_R, diff_R)) |>
  cols_label(
    starts_with("qb_rank") ~ 'QB rank',
    starts_with("def_rank") ~ 'Def rank',
    starts_with('diff') ~ '',
    starts_with("qb_id") ~ '',
    starts_with("qb_name") ~ 'Player',
    starts_with("team") ~ '',
  ) |>
  gtExtras::gt_theme_nytimes() |>
  gtExtras::gt_merge_stack(qb_name_L, qb_snaps_L) |>
  gtExtras::gt_merge_stack(qb_name_R, qb_snaps_R) |>
  gtExtras::gt_hulk_col_numeric(columns = starts_with('diff'), domain = range(combined$diff)) |>
  cols_width(
    everything() ~ "auto"
  ) |>
  tab_header(
    title = "NFL Week 79QB Betrayal Index",
    subtitle = 'Rank is based on EPA/play on designed runs & passes, excluding garbage time (<5% win)'
  ) |>
  tab_options(
    data_row.padding = gt::px(2)  # smaller = tighter rows
  ) |>
  tab_source_note("Data: @nflfastR | By: Joshios") |>
  gtsave('betrayal.png')

