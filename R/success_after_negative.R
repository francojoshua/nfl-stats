library(forcats)
library(nflfastR)
library(tidyverse)
pbp <- load_pbp(2025)


data <- pbp |>
  filter(down %in% 1:4 & !is.na(epa) & (rush == 1 | pass == 1)) |>
  filter(sack == 1 | (penalty == 1 & penalty_team == posteam & penalty_type == 'Offensive Holding')) |>
  filter(series_result != 'End of half') |>
  mutate(
    series_result = fct_collapse(
      series_result,
      "Success" = c("First down", "Touchdown"),
      "Settled" = c("Field goal", "Missed field goal"),
      "Lost Possession" = c("Turnover", "Turnover on downs", "Opp touchdown", "Safety"),
      "Punt" = c("Punt")
    ),
  ) |>
  group_by(team = posteam, series_result) |>
  summarize(
    frequency = n(),
  ) |>
  ungroup() |>
  group_by(team) |>
  mutate(
    amt = sum(frequency),
    frequency = frequency / amt,
    success_rate = sum(frequency[series_result == 'Success']),
  ) |>
  arrange(success_rate) |>
  ungroup() |>
  mutate(team = factor(team, levels = unique(team))) |>
  select(-amt, -success_rate)

data |>
  print(n=100)

data$series_result = factor(data$series_result, levels = c("Lost Possession", "Punt", "Settled", "Success"))
plot <- ggplot(data, aes(y=frequency, x = team, fill = series_result)) +
  geom_bar(position = 'stack', stat='identity', width = 0.8, linewidth = 2) +
  scale_fill_manual(
    values = c(
      "Lost Possession" = "#C62828",  # red
      "Punt"            = "#9E9E9E",  # gray
      "Settled"         = "#FBC02D",  # gold
      "Success"         = "#2E7D32"   # green
    )
  ) +
  labs(
    y = 'Frequency (%)',
    x = 'Team',
    title = '2025 NFL Offensive Series Success After Sack Taken or Offensive Holding',
    subtitle = 'Success = Touchdown/First down, Settled = FG attempt',
    caption = "Data: @nflfastR | Chart: Joshios",
    fill = "Result"
  ) +
  scale_y_continuous(labels = scales::percent) +
  coord_flip() +
  theme_minimal() +
  guides(fill = guide_legend(reverse = TRUE)) +
  theme(
    plot.title = ggplot2::element_text(face = "bold"),
    axis.text.y.left = nflplotR::element_nfl_logo(size = 0.75),
  )

plot

ggsave(plot=plot, 'success_after.png')


