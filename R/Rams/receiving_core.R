pbp <- load_pbp(2025)

roster <- load_rosters(2025)



rams_data <- roster |> 
  filter(team == 'LA') |>
  select(id = gsis_id, name = full_name)

receiving_data <- pbp |>
  filter(down %in% 1:4 & pass == 1) |>
  filter(posteam == 'LA') |>
  filter(!is.na(receiver_player_id)) |>
  group_by(id = receiver_player_id) |>
  summarize(
    targets = n(),
    rec = sum(complete_pass == 1),
    yards=sum(yards_gained),
    avg=ifelse(rec >0, yards/rec, 0),
    tds = sum(touchdown),
    epa = sum(epa),
    epa_per_rec = ifelse(rec > 0, epa / rec, 0),
    first_downs = sum(first_down),
    first_down_pct = ifelse(rec > 0, first_downs / rec, 0),
    yac = sum(yards_after_catch, na.rm = TRUE),
    yac_per_rec = ifelse(rec > 0, yac / rec, 0),
    rdz_targets = sum(yardline_100 <= 20),
    .groups = "drop"
  ) |>
  mutate(targetshare = targets / sum(targets)) |>
  arrange(-yards)

left_join(receiving_data, rams_data) |>
  slice_max(n = 12, order_by=yards) |>
  gt() |>
  nflplotR::gt_nfl_headshots(id) |>
  gtExtras::gt_theme_nytimes() |>
  fmt_percent(columns = c(targetshare, first_down_pct), decimals = 1) |>

  cols_label(
    id = '',
    targetshare = 'Tgt %',
    targets = 'Tgt',
    yards = 'yds',
    tds = 'TD',
    avg = 'Yds/Rec',
    first_downs = '1st downs',
    yac_per_rec = 'YAC/Rec',
    epa_per_rec = 'EPA/Rec',
    first_down_pct = '1st Down %',
    rdz_targets = "Rdz Tgt¹"
  ) |>
  fmt_number(columns=c(epa, avg, yac_per_rec, epa_per_rec), decimals=1) |>
  cols_move(name, after=id) |>
  cols_move(c(rec, yards, avg, tds), after=name) |>
  cols_move(c(targets, targetshare, rdz_targets, epa, epa_per_rec), after=tds) |> 
  gtExtras::gt_add_divider(columns=tds) |>
  gtExtras::gt_add_divider(columns=epa_per_rec) |>
  tab_header(
    title = "Rams receiving core through Week 7",
  ) |>
  data_color(
    columns = c(rec, yards, avg, tds, targetshare, targets, epa, first_downs, yac, yac_per_rec,epa_per_rec, first_down_pct, rdz_targets),
    method = "numeric",
    palette = "Greens",
  ) |>
  tab_source_note(
    source_note = "¹ Rdz Tgt = Target in the Redzone | Data: @nflfastR | By: Joshios"
  )
