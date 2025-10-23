stats <- load_pfr_advstats(stat_type = 'def', seasons=c(2025))


team_stats <- load_team_stats(summary_level = 'reg', seasons=c(2025))
tfls <- team_stats |> select(team, tfls = def_tackles_for_loss, ff=def_fumbles_forced, games)

tfls |> print(n=32)

glimpse(team_stats)
data <- stats |>
  mutate(team = if_else(team == "OAK", "LV", team)) |>
  select(team, def_ints, def_sacks, def_pressures, def_times_hitqb) |>
  group_by(team) |>
  summarize(
    ints = sum(def_ints, na.rm= TRUE),
    sacks = sum(def_sacks, na.rm = TRUE),
    pressures = sum(def_pressures, na.rm = TRUE),
  ) |>
  ungroup() |>
  left_join(tfls, by = c('team')) |>
  mutate(havoc = (ints + (ff*0.8) + (sacks*0.6) + pressures * 0.4 + tfls * 0.3) / games) |>
  select(-games) |>
  arrange(-havoc) |>
  mutate(rank = row_number())

data |> print(n = 32)

left <- data |> filter(rank <= 16)
right <- data |> filter(rank > 16)


left <- rename_with(left, ~ paste0(.x, "_L"))
right <- rename_with(right, ~ paste0(.x, "_R"))

combined <- bind_cols(left, right)


plot <- combined |> gt() |>
  gt_nfl_logos(starts_with("team")) |>
  cols_move_to_start(columns = c(rank_L, team_L, ints_L, ff_L, sacks_L, pressures_L, tfls_L, havoc_L)) |>
  cols_move(columns = c(rank_R, team_R, ints_R, ff_R, sacks_R, pressures_R, tfls_R, havoc_R), after=havoc_L) |>
  fmt_number(columns = starts_with("havoc"), decimals = 1) |>
  gtExtras::gt_theme_nytimes() |>
  tab_style(
    locations = cells_body(
      columns = starts_with('rank')
    ),
    style = cell_text(weight = 'bold')
  ) |>
  data_color(
    columns = starts_with('havoc'),
    palette = 'RdYlGn',
    domain = range(data$havoc)
  ) |>
  data_color(
    columns = starts_with('ints'),
    palette = 'RdYlGn',
    domain = range(data$ints)
  ) |>
  data_color(
    columns = starts_with('ff'),
    palette = 'RdYlGn',
    domain = range(data$ff)
  ) |>
  data_color(
    columns = starts_with('sacks'),
    palette = 'RdYlGn',
    domain = range(data$sacks)
  ) |>
  data_color(
    columns = starts_with('pressures'),
    palette = 'RdYlGn',
    domain = range(data$pressures)
  ) |>
  data_color(
    columns = starts_with('tfls'),
    palette = 'RdYlGn',
    domain = range(data$tfls)
  ) |>
  gtExtras::gt_add_divider(columns = havoc_L, style = 'solid', color ='black', weight = px(3)) |>
  gtExtras::gt_add_divider(columns = tfls_L, style = 'solid', color = 'black', weight = px(3)) |>
  gtExtras::gt_add_divider(columns = havoc_R, style = 'solid', color ='black', weight = px(3)) |>
  gtExtras::gt_add_divider(columns = tfls_R, style = 'solid', color = 'black', weight = px(3)) |>
  cols_label(
    starts_with('rank') ~ '',
    starts_with('team') ~ 'Team',
    starts_with('havoc') ~ 'Havoc',
    starts_with('ints') ~ 'Int',
    starts_with('ff') ~ 'FF',
    starts_with('sacks') ~ 'Sack',
    starts_with('pressures') ~ 'Pres.',
    starts_with('tfl') ~ 'TFL'
  ) |>
  tab_header(
    title = '2025 Week 1-6 Defensive Havoc Score',
    subtitle = 'Havoc rate is a weighted average in the following order of biggest weight to smallest weight recorded by defenses: interceptions, forced fumbles, sacks, pressures, tackles for loss. It is defined per game.'
  ) |>
  tab_footnote('Havoc rate = (Int + ⅘FF + ⅗Sack + ⅖Pressure + ³⁄₁₀TFL) / Games Played') |>
  tab_source_note(
    "Data: @nflfastR | Chart: Joshios"
  ) |>
  cols_width(
    everything() ~ px(50)
  ) |>
  tab_options(
    data_row.padding = gt::px(2)  # smaller = tighter rows
  )

plot

plot |> gtsave('havoc.png')  
  
