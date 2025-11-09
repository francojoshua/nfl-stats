pbp <- load_pbp(2015:2025)
pbp25 <- load_pbp(2025)


schedule25 <- load_schedules(2015:2025) |>
  clean_homeaway(invert = 'result') |>
  mutate(score = paste0(team_score, '-', opponent_score)) |>
  select(game_id, team, result, score)


scores <- pbp |>
  filter(down %in% 1:4 & score_differential >= 19) |> 
  group_by(game_id, team = posteam) |>
  summarize(
    max_lead = max(score_differential),
    opponent = first(defteam),
    .groups = 'drop'
  )

left_join(scores, schedule25, by = c('team', 'game_id')) |>
  mutate(win = ifelse(result > 0, 'W', 'L')) |>
  filter(team == 'LA') |>
  gt() |>
  nflplotR::gt_nfl_logos(columns = c(team, opponent)) |>
  gtExtras::gt_theme_nytimes() |>
  cols_hide(columns = c(result)) |>
  cols_move(columns = c(score, win, max_lead), after=opponent) |>
  cols_label(
    team = '',
    max_lead = 'Largest Lead',
    win = 'Result'
  ) |>
  tab_style(
    style = cell_text(color ='darkgreen', weight = 'bold'),
    locations = cells_body(
      columns = win,
      rows = win == 'W'
    )
  ) |>
  tab_style(
    style = cell_text(color ='red', weight = 'bold'),
    locations = cells_body(
      columns = win,
      rows = win == 'L'
    )
  ) |>
  gt::tab_header(
    title = 'Rams games with a 19+ point lead',
    subtitle = 'Games from 2020 - 2025'
  ) |>
  gt::tab_source_note(
    'Data: @nflfastR | By: Joshios'
  )

