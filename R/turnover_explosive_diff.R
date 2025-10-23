library(nflplotR)
library(ggplot2)
library(gt)
library(nflreadr)
library(dplyr, warn.conflicts = FALSE)
options(nflreadr.verbose = FALSE)

pbp <- load_pbp(2025)


explosive_plays <- pbp |> filter(down %in% 1:4 & (rush == 1 | pass == 1)) |>
  group_by(team = posteam) |>
  summarize(
    runs = sum(rushing_yards >= 10, na.rm = TRUE),
    passes = sum(passing_yards >= 20, na.rm = TRUE),
    total = runs + passes
  )

explosive_plays_allowed <- pbp |> filter(down %in% 1:4 & (rush == 1 | pass == 1)) |>
  group_by(team = defteam) |>
  summarize(
    runs_allowed = sum(rushing_yards >= 10, na.rm = TRUE),
    passes_allowed = sum(passing_yards >= 20, na.rm = TRUE),
    total_allowed = runs_allowed + passes_allowed
  )

giveaways <- pbp |> filter(down %in% 1:4) |>
  group_by(team = posteam) |>
  summarize(
    ga_interceptions = sum(interception, na.rm = TRUE),
    ga_fumbles_lost = sum(fumble_lost, na.rm = TRUE),
    ga_turnovers = ga_interceptions + ga_fumbles_lost
  )

takeaways <- pbp |> filter(down %in% 1:4) |>
  group_by(team = defteam) |>
  summarize(
    ta_interceptions = sum(interception, na.rm = TRUE),
    ta_fumbles_lost = sum(fumble_lost, na.rm = TRUE),
    ta_turnovers = ta_interceptions + ta_fumbles_lost
  )
  

takeaways
explosive_plays_allowed

turnovers_committed 

combined <- explosive_plays |>
  full_join(explosive_plays_allowed, by = 'team') |>
  full_join(giveaways, by = 'team') |>
  full_join(takeaways, by = 'team') |>
  mutate(
    turnover_differential = ta_turnovers - ga_turnovers,
    explosive_differential = total - total_allowed
  ) |>
  select(team, turnover_differential, explosive_differential)
  
  
plot <- ggplot2::ggplot(combined, aes(x = turnover_differential, y = explosive_differential)) +
  nflplotR::geom_mean_lines(aes(x0 = turnover_differential , y0 = explosive_differential)) +
  nflplotR::geom_nfl_logos(aes(team_abbr = team), width = 0.065, alpha = 0.7) +
  ggplot2::labs(
    x = "Turnover differential",
    y = "Explosive play differential",
    caption = "Data: @nflfastR | Joshios",
    subtitle = "Explosive plays (10+ yard run, 20+ yard pass) vs turnovers (interception, fumble lost), differentials compared to their opponents",
    title = "2025 NFL Explosive Play and Turnover Differentials"
  ) +
  ggplot2::theme_minimal() +
  # quadrant text labels
  annotate("text", x = max(combined$turnover_differential, na.rm=TRUE) * 0.75,
           y = max(combined$explosive_differential, na.rm=TRUE) * 0.75,
           label = "Win explosive + win turnovers", fontface="bold", color="darkgreen") +
  annotate("text", x = min(combined$turnover_differential, na.rm=TRUE) * 0.75,
           y = max(combined$explosive_differential, na.rm=TRUE) * 0.75,
           label = "Win explosive + lose turnovers", fontface="bold", color="orange") +
  annotate("text", x = min(combined$turnover_differential, na.rm=TRUE) * 0.75,
           y = min(combined$explosive_differential, na.rm=TRUE) * 0.75,
           label = "Lose both battles", fontface="bold", color="red") +
  annotate("text", x = max(combined$turnover_differential, na.rm=TRUE) * 0.75,
           y = min(combined$explosive_differential, na.rm=TRUE) * 0.75,
           label = "Lose explosive + win turnovers", fontface="bold", color="blue") +
  ggplot2::theme(
    plot.title = ggplot2::element_text(face = "bold"),
    plot.title.position = "plot",
    plot.background = ggplot2::element_rect(fill = "#F0F0F0")
  )
plot
ggsave(plot, filename='diff.png')
