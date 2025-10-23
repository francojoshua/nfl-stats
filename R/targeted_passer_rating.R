calculate_stats() |>
   glimpse()

stats <- calculate_stats()



stats |>
  select(name = player_display_name, target_share) |>
  mutate(target_share = target_share * 100) |>
  arrange(-target_share)


conv <- calculate_series_conversion_rates(pbp)

conv |> arrange(-off_scr_4th)


calc_passer_rating <- function(cmp, att, yds, td, int) {
  att_safe <- ifelse(att == 0, NA_real_, att)

  a <- ((cmp / att_safe) - 0.3) * 5
  b <- ((yds / att_safe) - 3) * 0.25
  c <- (td / att_safe) * 20
  d <- 2.375 - ((int / att_safe) * 25)

  clamp <- function(x) pmax(0, pmin(x, 2.375))
  ((clamp(a) + clamp(b) + clamp(c) + clamp(d)) / 6) * 100  # returns 158.3333... at max
}


pbp |>
  filter(down %in% 1:4 & !is.na(receiver_player_id)) |>
  group_by(receiver_player_id, receiver_player_name) |>
  summarize(
    team = last(posteam),
    att = n(),
    cmp = sum(complete_pass == 1, na.rm = TRUE),
    yds = sum(yards_gained, na.rm = TRUE),
    td = sum(touchdown, na.rm = TRUE),
    int = sum(interception, na.rm = TRUE),
    passer_rating = calc_passer_rating(cmp, att, yds, td, int)
  ) |>
  ungroup() |>
  filter(att >= 20) |>
  arrange(-passer_rating) |>
  glimpse()
