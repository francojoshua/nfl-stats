library(tidyverse)
library(nflplotR)
library(gt)
library(gtExtras)

pbp <- load_pbp(2025)
pbp |>
  filter(down %in% 1:4 & air_yards >= 15) |>
  group_by(id = passer_player_id) |>
  summarize(name = first(passer_player_name), 
            plays = n(), 
            completions = sum(complete_pass),
            epa = mean(epa),
            catt = paste0(completions, '/', plays),
            yards = sum(passing_yards, na.rm = TRUE),
            tds = sum(touchdown), 
            ints = sum(interception), 
            ratio = completions/plays) |>
  filter(plays >= 10) |>
  slice_max(n = 16, order_by=epa) |>
  mutate(index = row_number()) |>
  gt() |>
  gt::tab_header(
    title = 'Passes over 15+ air yards',
    subtitle = 'Ranked by EPA/Play (minimum 10 plays over 15 air yards)'
  ) |>
  gt::fmt_percent(columns=('ratio'), decimals = 1) |>
  gt::fmt_number(columns=('epa'), decimals =2 ) |>
  gt::cols_move_to_start(columns=c('index')) |>
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(columns = index)
  ) |>
  gt::cols_label(
    id = '',
    index= '',
    epa = 'EPA/play',
    name = 'Passer',
    ratio = 'Pct (%)',
    ints = 'Int',
    tds = 'TD',
    yards = 'Yds',
    catt = 'C/ATT'
  ) |>
  gt::cols_hide(columns = c('plays', 'completions')) |>
  gt::tab_options(
    data_row.padding = px(1)
  ) |>
  gt_nfl_headshots(columns = c('id')) |>
  gt::tab_source_note(source_note='Data: @nflfastR | Made by Joshios') |>
  gtExtras::gt_theme_nytimes() |>
  tab_options(
    data_row.padding = gt::px(2)  # smaller = tighter rows
  )

