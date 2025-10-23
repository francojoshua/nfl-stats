pbp <- load_pbp(2024:2025)


success_rate_2025 <- pbp |>
  filter(season == 2025 & season_type == 'REG') |>
  filter(down %in% 1:4 & !is.na(epa) & (pass == 1 | rush == 1)) |>
  group_by(team = defteam) |>
  summarize(
    success_rate = mean(success)
  ) |>
  arrange(success_rate) |>
  ungroup() |>
  mutate(rank = row_number())



success_rate_2024 <- pbp |>
  filter(season == 2024 & season_type == 'REG') |>
  filter(down %in% 1:4 & !is.na(epa) & (pass == 1 | rush == 1)) |>
  group_by(team = defteam) |>
  summarize(
    success_rate...2 = mean(success)
  ) |>
  arrange(success_rate...2) |>
  ungroup() |>
  mutate(rank...2 = row_number())


success_rate <- full_join(success_rate_2025, success_rate_2024)


change_success_rate <- success_rate |>
  mutate(change = rank...2 - rank) |>
  select(-success_rate...2, -rank...2)


left <- change_success_rate |> filter(rank <= 16)
right <- change_success_rate |> filter(rank > 16)


left <- left |> rename_with(~ paste0(.x, "_L"))
right <- right |> rename_with(~ paste0(.x, "_R"))

sr_range <- range(
  c(success_rate_2025$success_rate)
)

sr_range


data <- bind_cols(left, right)

plot <- data |>
  gt() |>
  nflplotR::gt_nfl_logos(columns = starts_with('team')) |>
  fmt_number(columns = starts_with("success_rate"), decimals = 2) |>
  cols_move_to_start(rank_L) |>
  cols_move(columns = c(rank_R), after=change_L) |>
  cols_label(
    rank_L = 'Rank',
    team_L = 'Team',
    success_rate_L ='SR',
    change_L = 'Change',
    rank_R = 'Rank',
    team_R = 'Team',
    success_rate_R = 'SR',
    change_R = 'Change'
  ) |>
  gtExtras::gt_add_divider(columns = change_L, style = 'dashed') |>
  tab_header(
    title = '2025 Defensive Success Rate Allowed',
    subtitle = 'SR is the success rate allowed and the change is the change is rank from last season'
  ) |>
  tab_source_note(
    "Data: @nflfastR | Chart: Joshios"
  ) |>
  gtExtras::gt_theme_nytimes() |>
  text_transform(
    locations = cells_body(columns = starts_with('change')),
    fn = function(x) {
      vals <- as.numeric(x)
      ifelse(
        vals > 0, paste0("<span style='color:green'>↑ ", x, "</span>"),
        ifelse(vals < 0, paste0("<span style='color:red'>↓ ", abs(vals), "</span>"), paste0('— ', x))
      )
    }
  ) |>
  data_color(
    columns = starts_with('success_rate'),
    method = "numeric",
    palette = "RdYlGn",
    domain = sr_range,
    reverse = TRUE
  ) |>
  tab_style(
    locations = cells_body(
      columns = c(starts_with('rank'))
    ),
    style = cell_text(weight = 'bold')
  ) |>
  cols_width(
    everything() ~ px(55)
  )

