library(dplyr)
library(tidyr)
library(ggplot2)

pbp <- load_pbp(2025)


drive_ratios <- pbp |>
  filter(down %in% 1:4 & yardline_100 <= 20) |>
  group_by(game_id, drive, team = posteam) |>
  summarize(
    result = last(series_result),
    plays = n(),
    yards = sum(yards_gained, na.rm = TRUE),
    penalties = sum(penalty, na.rm = TRUE),
    touchdown = any(touchdown == 1 & td_team == team, na.rm = TRUE),
  ) |>
  filter(result != 'QB kneel') |>
  group_by(team) |>
  summarize(
    drives = n(),
    td_ratio = mean(result == 'Touchdown' | touchdown == 1),
    fg_ratio = mean(result == 'Field goal'),
    to_ratio = mean(result == 'Turnover' | result == 'Opp touchdown' | result == "Turnover on downs"),
    mfg_ratio = mean(result == 'Missed field goal'),
    end_ratio = mean(result == 'End of half'),
    penalties = mean(penalties > 0)
  ) |>
  arrange(td_ratio) |>
  print(n = 32)

order <- drive_ratios |> pull("team")



drive_ratios_long <- drive_ratios %>%
  pivot_longer(
    cols = ends_with("_ratio"),
    names_to = "result_type",
    values_to = "ratio"
  ) |>
  mutate(result_type = factor(result_type,
                            levels = c("end_ratio", "mfg_ratio", "to_ratio", "fg_ratio", "td_ratio"),
                            labels = c("End of half", "Missed field goal", "Turnover", "Field goal", "Touchdown")),
         team = factor(team, levels=order))

         
drive_ratios_long

p<- ggplot(drive_ratios_long, aes(x = ratio, y = team, fill = result_type)) +
  geom_col(position = "stack", width = 0.8, linewidth = 2) +
  labs(
    title = "NFL 2025 Red Zone Drives - Result by Team",
    subtitle = "Occurs when an offense runs at least one play inside the opponent’s 20-yard line (excluding QB kneels)",
    caption = "Data: @nflfastR | Joshios",
    x = "Drive Proportion (%)",
    y = "Team",
    fill = "Result"
  ) +
  scale_fill_manual(
    breaks = c("Touchdown", "Field goal", "Turnover", "Missed field goal", "End of half"),
    values = c("Touchdown" = "#1b9e77", 
               "Turnover" = "#e41a1c", 
               "Field goal" = "#e6ab02",
               "Missed field goal" = "brown",
               "End of half" = "darkgray")  # if you have punts too
  ) +
  theme_minimal() +
  theme(
    plot.title = ggplot2::element_text(face = "bold"),
    axis.text.y.left = nflplotR::element_nfl_logo(size = 0.8),
  )

p
p

ggsave(filename="redzone.png", plot = p)
