library(tidyverse)
library(nflfastR)
library(nflverse)

rosters <- load_rosters(2000:2023)

rosters_clean <- rosters |> select(gsis_id, season, full_name, position, height, weight)

#### QB ####
qb_function <- function(min_year, max_year) {

pbp <- nflreadr::load_pbp(min_year:max_year)

pbp_clean <- pbp |> 
  left_join(rosters_clean, by = c("id" = "gsis_id", "season")) |> 
  filter(position == "QB")

qb_pbp <- pbp_clean |> group_by(id, season) |> 
  summarise(name = last(name), 
            plays = sum(pass[!is.na(pass)]) + sum(rush[!is.na(rush)]), 
            height = last(height), position = last(position))

qb_data <- qb_pbp |>  group_by(season) |> filter(!is.na(height)) |>
  summarise(mean_height = weighted.mean(height, plays), 
            pos = last(position))

return(qb_pbp)
}

qb_data_00 <- qb_function(2000, 2009)
qb_data_10 <- qb_function(2010, 2019)
qb_data_20 <- qb_function(2020, 2023)

qb_data_dec <- rbind(qb_data_00, qb_data_10, qb_data_20)

#### WR, TE ####

rec_function <- function(min_year, max_year, pos) {
player_stats <- load_player_stats(seasons = min_year:max_year)

clean_player_stats <- player_stats |> 
  group_by(player_id, season) |> 
  summarise(player_id = last(player_id), 
            name = last(player_name), 
            rec = sum(receptions))

merged <- clean_player_stats |> 
  left_join(rosters_clean, by = c("player_id" = "gsis_id", "season")) |> 
  filter(position == pos)

data <- merged |>  group_by(season) |> 
  summarise(mean_height = weighted.mean(height, rec), 
            mean_weight = weighted.mean(weight, rec), 
            pos = last(position))

}

# WR 
wr_data_00 <- rec_function(2000, 2009, "WR")
wr_data_10 <- rec_function(2010, 2019, "WR")
wr_data_20 <- rec_function(2020, 2023, "WR")

wr_data <- rbind(wr_data_00, wr_data_10, wr_data_20)

# TE
te_data_00 <- rec_function(2000, 2009, "TE")
te_data_10 <- rec_function(2010, 2019, "TE")
te_data_20 <- rec_function(2020, 2023, "TE")

te_data <- rbind(te_data_00, te_data_10, te_data_20)

#### RB ####
run_function <- function(min_year, max_year, pos) {
  player_stats <- load_player_stats(seasons = min_year:max_year)
  
  clean_player_stats <- player_stats |> 
    group_by(player_id, season) |> 
    summarise(player_id = last(player_id), 
              name = last(player_name), 
              carries = sum(carries))
  
  merged <- clean_player_stats |> 
    left_join(rosters_clean, by = c("player_id" = "gsis_id", "season")) |> 
    filter(position == pos)
  
  data <- merged |>  group_by(season) |> 
    summarise(mean_height = weighted.mean(height, carries), 
              mean_weight = weighted.mean(weight, carries), 
              pos = last(position))
  
}

rb_data_00 <- run_function(2000, 2009, "RB")
rb_data_10 <- run_function(2010, 2019, "RB")
rb_data_20 <- run_function(2020, 2023, "RB")

rb_data <- rbind(rb_data_00, rb_data_10, rb_data_20)

#### GRAPHING ####

library(sysfonts)
library(showtext)
font_add_google(name = "Sora")
showtext_auto()

graph <- function(data, position){
  data |> ggplot(aes(x = season, y = mean_height)) + 
    geom_line() + 
    geom_point() +
    scale_x_continuous(breaks = seq(2000, 2023, 10)) +
    labs(
      title = paste0("NFL ", position, " Average Height by Season"),
      subtitle = "2000-2023",
      x = "Year",
      y = "Mean Height (inches)",
      caption = "Data: nflverse. Analysis: Makenna Hack."
    ) + 
    theme_minimal() + 
    theme(
      text = element_text(family = "Sora"), 
      plot.title = element_text(face = "bold", size = 20),
      plot.subtitle = element_text(face = "bold", size = 15), 
      axis.text = element_text(face = "bold", size = 15), 
      axis.title = element_text(face = "bold", size = 15),
      plot.background = element_rect(fill = "white"), 
      panel.border = element_blank()
    )
}

qb_plot <- graph(qb_data, "Quarterbacks'")
wr_plot <- graph(wr_data, "Wide Receivers'")
te_plot <- graph(te_data, "Tight Ends'")
rb_plot <- graph(rb_data, "Running Backs'")

ggsave(filename = "C:/Users/maken/Desktop/personal_website/off_heights_post/qb_plot.png", 
       plot = qb_plot, height = 3.6, width = 3.74)
ggsave(filename = "C:/Users/maken/Desktop/personal_website/off_heights_post/wr_plot.png", 
       plot = wr_plot, height = 3.6, width = 3.74)
ggsave(filename = "C:/Users/maken/Desktop/personal_website/off_heights_post/te_plot.png", 
       plot = te_plot, height = 3.6, width = 3.74)
ggsave(filename = "C:/Users/maken/Desktop/personal_website/off_heights_post/rb_plot.png", 
       plot = rb_plot, height = 3.6, width = 3.74)

qb_data_dec$decade <- ifelse(qb_data$season <= 2009, "2000s",
                      ifelse(qb_data$season >= 2010 & qb_data$season <= 2019, "2010s",
                             ifelse(qb_data$season >= 2020, "2020s", NA)))

wr_data_dec$decade <- ifelse(wr_data_dec$season <= 2009, "2000s",
                             ifelse(wr_data_dec$season >= 2010 & wr_data_dec$season <= 2019, "2010s",
                                    ifelse(wr_data_dec$season >= 2020, "2020s", NA)))
te_data_dec$decade <- ifelse(te_data_dec$season <= 2009, "2000s",
                             ifelse(te_data_dec$season >= 2010 & te_data_dec$season <= 2019, "2010s",
                                    ifelse(te_data_dec$season >= 2020, "2020s", NA)))
rb_data_dec$decade <- ifelse(rb_data_dec$season <= 2009, "2000s",
                             ifelse(rb_data_dec$season >= 2010 & rb_data_dec$season <= 2019, "2010s",
                                    ifelse(rb_data_dec$season >= 2020, "2020s", NA)))



qb_data_dec <- qb_data_dec |> dplyr::filter(!is.na(height)) |> ungroup()
qb_mean_by_dec <- qb_data_dec |> group_by(decade) |> summarise(mean_dec = mean(height))

wr_data_dec <- wr_data_dec |> dplyr::filter(!is.na(height)) |> ungroup()
wr_mean_by_dec <- wr_data_dec |> group_by(decade) |> summarise(mean_dec = mean(height))

te_data_dec <- te_data_dec |> dplyr::filter(!is.na(height)) |> ungroup()
te_mean_by_dec <- te_data_dec |> group_by(decade) |> summarise(mean_dec = mean(height))

rb_data_dec <- rb_data_dec |> dplyr::filter(!is.na(height)) |> ungroup()
rb_mean_by_dec <- rb_data_dec |> group_by(decade) |> summarise(mean_dec = mean(height))


decade_heights <- function(data, var, by_vars, weights = NULL) {
  data |> 
    count({{ by_vars }}, {{ var }}, wt = {{ weights }}) |> 
    mutate(
      .by = {{ by_vars }},
      pct = n / sum(n))
}

qb_heights_decades <- decade_heights(qb_data_dec, height, by_vars = decade, weights = plays)
wr_heights_decades <- decade_heights(wr_data_dec, height, by_vars = decade, weights = rec)
te_heights_decades <- decade_heights(te_data_dec, height, by_vars = decade, weights = rec)
rb_heights_decades <- decade_heights(rb_data_dec, height, by_vars = decade, weights = carries)

dec_graph <- function(data, mean_data, position, play){
  data |> ggplot(aes(height, pct)) + 
    geom_line() + 
    geom_vline(data = mean_data, aes(xintercept = mean_dec), linetype = "dashed") +
    facet_wrap(~ decade, ncol = 1, strip.position = "right") +
    scale_y_continuous(labels = label_percent()) +
    labs(
      title = paste0(position, " Height Distriubtion by Decade"),
      subtitle = "Average Height per Decade Shown by Dashed Line",
      x = "Height (inches)",
      y = paste0("% of ", play),
      caption = "Data: nflverse. Analysis: Makenna Hack."
    )  + theme_minimal() + 
    theme(
      text = element_text(family = "Sora"), 
      plot.title = element_text(face = "bold", size = 20),
      plot.subtitle = element_text(face = "bold", size = 15), 
      axis.text = element_text(face = "bold", size = 15), 
      axis.title = element_text(face = "bold", size = 15),
      plot.background = element_rect(fill = "white"), 
      panel.border = element_blank()
    )
}

qb_dec <- dec_graph(qb_heights_decades, qb_mean_by_dec, "Quarterback", "Total Plays")
wr_dec <- dec_graph(wr_heights_decades, wr_mean_by_dec, "Wide Receiver", "Total Receptions")
te_dec <- dec_graph(te_heights_decades, te_mean_by_dec, "Tight End", "Total Receptions")
rb_dec <- dec_graph(rb_heights_decades, rb_mean_by_dec, "Running Back", "Total Carries")

ggsave(filename = "C:/Users/maken/Desktop/personal_website/off_heights_post/qb_dec.png", 
       plot = qb_dec, height = 3.6, width = 3.74)
ggsave(filename = "C:/Users/maken/Desktop/personal_website/off_heights_post/wr_dec.png", 
       plot = wr_dec, height = 3.6, width = 3.74)
ggsave(filename = "C:/Users/maken/Desktop/personal_website/off_heights_post/te_dec.png", 
       plot = te_dec, height = 3.6, width = 3.74)
ggsave(filename = "C:/Users/maken/Desktop/personal_website/off_heights_post/rb_dec.png", 
       plot = rb_dec, height = 3.6, width = 3.74)

