library(gt)
library(gtExtras)
pbp <- load_pbp(2024:2025)


def_epa <- pbp |>
  filter(season_type == 'REG') |>
  filter(vegas_wp > 0.02) |>
  filter(down %in% 1:4 & !is.na(epa) & (pass == 1 | rush == 1)) |>
  group_by(season, team = defteam) |>
  summarize(
    epa = mean(epa)
  ) |>
  ungroup() |>
  group_by(season) |>
  arrange(epa) |>
  mutate(rank = row_number()) |>
  ungroup() |>
  pivot_wider(
    names_from = season,
    values_from = c(epa, rank)
  ) |>
  mutate(change = rank_2024 - rank_2025) |>
  select(-rank_2024, -epa_2024) |>
  rename(rank = rank_2025, epa = epa_2025) |>
  arrange(rank)

def_epa

left <- def_epa |> filter(rank <= 16)
right <- def_epa |> filter(rank > 16)


left <- left |> rename_with(~ paste0(.x, "_L"))
right <- right |> rename_with(~ paste0(.x, "_R"))

data <- bind_cols(left, right)

plot <- data |>
  gt() |>
  nflplotR::gt_nfl_logos(columns = starts_with('team')) |>
  fmt_number(columns = starts_with("epa"), decimals = 3) |>
  cols_move_to_start(rank_L) |>
  cols_move(columns = c(rank_R), after=change_L) |>
  cols_label(
    starts_with('rank') ~ "Rank",
    starts_with("team") ~ "Team",
    starts_with("epa") ~ "EPA",
    starts_with("Change") ~ "Change"
  ) |>
  gtExtras::gt_add_divider(columns = change_L, style = 'dashed') |>
  tab_header(
    title = '2025 Defensive EPA through Week 8',
    subtitle = 'Measured in EPA/play (excluding <2% win prob) and the change is the change is rank from last season'
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
    columns = starts_with('epa'),
    method = "numeric",
    palette = "RdYlGn",
    domain = range(def_epa$epa),
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
  ) |>
  tab_options(
    data_row.padding = gt::px(2)  # smaller = tighter rows
  )

plot

plot |> gtsave('sr.png')
