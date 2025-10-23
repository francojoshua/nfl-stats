library(nflfastR)
library(nflreadr)
library(dplyr)
library(gt)

pbp <- load_pbp(2024:2025)


game_data <- pbp |> 
  filter(down %in% 1:4 & defteam == 'LA') |>
  group_by(game_id, team = defteam) |>
  summarize(rush_yards = sum(rushing_yards, na.rm = TRUE))
  


schedule <- load_schedules(2024:2025) |> 
  clean_homeaway(invert='result') |>
  filter(team == 'LA' & !is.na(result)) |>
  filter(team_qb_id == '00-0026498') |>
  filter(season == 2025 | (season == 2024 & week >= 11))

analysis <- schedule |> summarize(
  wins = sum(result > 0), 
  losses = sum(result < 0),
  point_differential = sum(result)
)



schedule_data <- left_join(schedule, game_data) |>
  mutate(
    score = paste0(team_score, '-', opponent_score),
    outcome = ifelse(result > 0, "W", "L")
  ) |>
  select(team, opponent, outcome, score, rush_yards)

bind_rows(schedule_data, analysis)

schedule_data |> gt() |>
  nflplotR::gt_nfl_logos(columns= c('team', 'opponent')) |>
  gt::tab_header(
    title = 'The Rams and their kryptonite',
    subtitle = 'The last 12 games that Matthew Stafford started for the Rams'
  ) |>
  data_color(
    columns = c(rush_yards),
    method = "numeric",
    palette = "RdYlGn",
    reverse = TRUE,
    domain = c(50, 314)
  ) |>
  gt::tab_source_note("Data: @nflfastR | By: Joshios") |>
  gtExtras::gt_theme_nytimes() |>
  gt::cols_label(
    rush_yards = 'Opp. Rush Yds'
  ) |>
  tab_style(
    style = cell_text(color='darkgreen', weight = 'bold'),
    locations = cells_body(
      columns = outcome,
      rows = outcome == 'W'
    )
  ) |>
  tab_style(
    style = cell_text(color = 'red', weight = 'bold'),
    locations = cells_body(
      columns = outcome,
      rows = outcome == 'L'
    )
  )
