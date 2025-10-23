library(nflfastR)
library(dplyr)
library(nflplotR)
library(gt)
library(gtExtras)

pbp <- nflfastR::load_pbp(2025)

big_pass <- pbp |>
  filter(down %in% 1:4 &pass == 1 & yards_gained >= 20) |>
  group_by(team = posteam) |>
  summarize(rec = n())

big_rush <- pbp |>
  filter(down %in% 1:4 & rush == 1 & yards_gained >= 10) |>
  group_by(team = posteam) |>
  summarize(rush = n())


big_pass_allowed <- pbp |>
  filter(down %in% 1:4 &pass == 1) |>
  group_by(team = defteam) |>
  summarize(rec = sum(yards_gained >= 20, na.rm = TRUE))

big_rush_allowed <- pbp |>
  filter(down %in% 1:4 & rush == 1) |>
  group_by(team = defteam) |>
  summarize(rush = sum(yards_gained >= 10, na.rm = TRUE))


big_plays <- left_join(big_rush, big_pass, by='team') |>
  mutate(plays = rec + rush) |>
  arrange(-plays)

big_plays_allowed <- left_join(big_rush_allowed, big_pass_allowed, by='team') |>
  mutate(plays = rec + rush) |>
  arrange(plays)

big_rush_allowed

bind_cols(big_plays, big_plays_allowed) |>
  mutate(rank...1 = row_number(), rank...2 = row_number()) |>
  gt() |>
  cols_label(
    team...1 = 'Team',
    rush...2 = 'Rush (10+)',
    rec...3 = 'Rec (20+)',
    plays...4 = 'Big Plays',
    team...5 = 'Team',
    rush...6 = 'Rush (10+)',
    rec...7 = 'Rec (20+)',
    plays...8 = 'Big Plays',
    rank...1 = 'Rank',
    rank...2 = 'Rank'
  ) |>
  gt_nfl_logos(columns = c(starts_with("team"))) |>
  cols_move(columns = c(rank...2), after=plays...4) |>
  cols_move_to_start(rank...1) |>
  gt_add_divider(columns = c(4), style = "dashed", weight = px(4)) |>
  tab_header(
    title = "Big Plays in Week 1-6",       # goes left
    subtitle = "Rushing plays of 10+ yards and passing plays of 20+ yards, scored and allowed"        # goes right
  ) |>
  tab_spanner(
    label = "Big Plays Made",
    columns = c(team...1, rush...2, rec...3, plays...4)
  ) |>
  tab_spanner(
    label = "Big Plays Allowed",
    columns = c(team...5, rush...6, rec...7, plays...8)
  ) |>
  data_color(
    columns = c(rush...2, rec...3, plays...4),
    method = "numeric",
    palette = "RdYlGn"
    ) |>
  data_color(
    columns = c(rush...6, rec...7, plays...8),
    method = "numeric",
    palette = "RdYlGn",
    reverse=TRUE
  ) |>
  tab_source_note("Data: @nflreadR | By: Joshios") |>
  gtExtras::gt_theme_nytimes() |>
  tab_style(
    style = cell_text(weight = "bold"),
    locations = list(
      cells_body(columns = c(rank...1, rank...2)),
      cells_column_labels(columns = c(rank...1, rank...2))   # <- bold the header too if you want
    )
  )

