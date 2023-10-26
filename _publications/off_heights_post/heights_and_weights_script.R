library(tidyverse)
library(nflfastR)
library(nflverse)

rosters <- load_rosters(2000:2023)

rosters_clean <- rosters |> select(gsis_id, season, full_name, position, height, weight)

# do all offensive skill pos, QB WR TE RB, by different snap counts, so rushes, routes, plays, etc. 

# function to grab data for each position 

#### 2020s ####

# QB 
pbp <- nflreadr::load_pbp(2020:2023)

pbp_clean <- pbp |> 
  left_join(rosters_clean, by = c("id" = "gsis_id", "season")) |> 
  filter(position == "QB")

qb_pbp <- pbp_clean |> group_by(id, season) |> 
  summarise(name = last(name), 
            plays = sum(pass[!is.na(pass)]) + sum(rush[!is.na(rush)]), 
            height = last(height), weight = last(weight), position = last(position))

qb_data <- qb_pbp |>  group_by(season) |> filter(!is.na(height), !is.na(weight)) |>
  summarise(mean_height = weighted.mean(height, plays), 
            mean_weight = weighted.mean(weight, plays), 
            pos = last(position))

# WR 
player_stats <- load_player_stats(seasons = 2020:2023)

clean_player_stats <- player_stats |> 
  group_by(player_id, season) |> 
  summarise(player_id = last(player_id), 
            name = last(player_name), 
            rec = sum(receptions))

wr_pbp <- clean_player_stats |> 
  left_join(rosters_clean, by = c("player_id" = "gsis_id", "season")) |> 
  filter(position == "WR")

wr_data <- wr_pbp |>  group_by(season) |> 
  summarise(mean_height = weighted.mean(height, rec), 
            mean_weight = weighted.mean(weight, rec), 
            pos = last(position))

# TE 
player_stats <- load_player_stats(seasons = 2020:2023)

clean_player_stats <- player_stats |> 
  group_by(player_id, season) |> 
  summarise(player_id = last(player_id), 
            name = last(player_name), 
            rec = sum(receptions))

te_pbp <- clean_player_stats |> 
  left_join(rosters_clean, by = c("player_id" = "gsis_id", "season")) |> 
  filter(position == "TE")

te_data <- te_pbp |>  group_by(season) |> 
  summarise(mean_height = weighted.mean(height, rec), 
            mean_weight = weighted.mean(weight, rec), 
            pos = last(position))

# RB
player_stats <- load_player_stats(seasons = 2020:2023)

clean_player_stats <- player_stats |> 
  group_by(player_id, season) |> 
  summarise(player_id = last(player_id), 
            name = last(player_name), 
            rec = sum(receptions))

rb_pbp <- clean_player_stats |> 
  left_join(rosters_clean, by = c("player_id" = "gsis_id", "season")) |> 
  filter(position == "RB")

rb_data <-rb_pbp |>  group_by(season) |> 
  summarise(mean_height = weighted.mean(height, carries), 
            mean_weight = weighted.mean(weight, carries), 
            pos = last(position))



#### 2010s ####

# QB 
pbp_10 <- nflreadr::load_pbp(2010:2019)

pbp_clean_10 <- pbp_10 |> 
  left_join(rosters_clean, by = c("id" = "gsis_id", "season")) |> 
  filter(position == "QB")

qb_pbp_10 <- pbp_clean_10 |> group_by(id, season) |> 
  summarise(name = last(name), 
            plays = sum(pass[!is.na(pass)]) + sum(rush[!is.na(rush)]), 
            height = last(height), weight = last(weight), position = last(position))

qb_data_10 <- qb_pbp_10 |>  group_by(season) |> filter(!is.na(height), !is.na(weight)) |>
  summarise(mean_height = weighted.mean(height, plays), 
            mean_weight = weighted.mean(weight, plays), 
            pos = last(position))

# WR 
player_stats_10 <- load_player_stats(seasons = 2010:2019)

clean_player_stats_10 <- player_stats_10 |> 
  group_by(player_id, season) |> 
  summarise(player_id = last(player_id), 
            name = last(player_name), 
            rec = sum(receptions))

wr_pbp_10 <- clean_player_stats_10 |> 
  left_join(rosters_clean, by = c("player_id" = "gsis_id", "season"))|> 
  filter(position == "WR")

wr_data_10 <- wr_pbp_10 |>  group_by(season) |> 
  summarise(mean_height = weighted.mean(height, rec), 
            mean_weight = weighted.mean(weight, rec), 
            pos = last(position))

# TE 
player_stats_10 <- load_player_stats(seasons = 2010:2019)

clean_player_stats_10 <- player_stats_10 |> 
  group_by(player_id, season) |> 
  summarise(player_id = last(player_id), 
            name = last(player_name), 
            rec = sum(receptions))

te_pbp_10 <- clean_player_stats_10 |> 
  left_join(rosters_clean, by = c("player_id" = "gsis_id", "season")) |> 
  filter(position == "TE")

te_data_10 <- te_pbp_10 |>  group_by(season) |> 
  summarise(mean_height = weighted.mean(height, rec), 
            mean_weight = weighted.mean(weight, rec), 
            pos = last(position))

# RB
player_stats_10 <- load_player_stats(seasons = 2010:2019)

clean_player_stats_10 <- player_stats_10 |> 
  group_by(player_id, season) |> 
  summarise(player_id = last(player_id), 
            name = last(player_name), 
            rec = sum(receptions))

rb_pbp_10 <- clean_player_stats_10 |> 
  left_join(rosters_clean, by = c("player_id" = "gsis_id", "season"))|> 
  filter(position == "RB")

rb_data_10 <- rb_pbp_10 |>  group_by(season) |> 
  summarise(mean_height = weighted.mean(height, carries), 
            mean_weight = weighted.mean(weight, carries), 
            pos = last(position))



#### 2000s ####

# QB 
pbp_00 <- nflreadr::load_pbp(2000:2009)

pbp_clean_00 <- pbp_00 |> 
  left_join(rosters_clean, by = c("id" = "gsis_id", "season")) |> 
  filter(position == "QB")

qb_pbp_00 <- pbp_clean_00 |> group_by(id, season) |> 
  summarise(name = last(name), 
            plays = sum(pass[!is.na(pass)]) + sum(rush[!is.na(rush)]), 
            height = last(height), weight = last(weight), position = last(position))

qb_data_00 <- qb_pbp_00 |>  group_by(season) |> filter(!is.na(height), !is.na(weight)) |>
  summarise(mean_height = weighted.mean(height, plays), 
            mean_weight = weighted.mean(weight, plays), 
            pos = last(position))

# WR 
player_stats_00 <- load_player_stats(seasons = 2000:2009)

clean_player_stats_00 <- player_stats_00 |> 
  group_by(player_id, season) |> 
  summarise(player_id = last(player_id), 
            name = last(player_name), 
            carries = sum(carries))

wr_pbp_00 <- clean_player_stats_00 |> 
  left_join(rosters_clean, by = c("player_id" = "gsis_id", "season")) |> 
  filter(position == "WR")

wr_data_00 <- wr_pbp_00 |>  group_by(season) |> 
  summarise(mean_height = weighted.mean(height, rec), 
            mean_weight = weighted.mean(weight, rec), 
            pos = last(position))

# TE 
player_stats_00 <- load_player_stats(seasons = 2000:2009)

clean_player_stats_00 <- player_stats_00 |> 
  group_by(player_id, season) |> 
  summarise(player_id = last(player_id), 
            name = last(player_name), 
            carries = sum(carries))

te_pbp_00 <- clean_player_stats_00 |> 
  left_join(rosters_clean, by = c("player_id" = "gsis_id", "season")) |> 
  filter(position == "TE")

te_data_00 <- te_pbp_00 |>  group_by(season) |> 
  summarise(mean_height = weighted.mean(height, rec), 
            mean_weight = weighted.mean(weight, rec), 
            pos = last(position))

# RB
player_stats_00 <- load_player_stats(seasons = 2000:2009)

clean_player_stats_00 <- player_stats_00 |> 
  group_by(player_id, season) |> 
  summarise(player_id = last(player_id), 
            name = last(player_name), 
            carries = sum(carries))

rb_pbp_00 <- clean_player_stats_00 |> 
  left_join(rosters_clean, by = c("player_id" = "gsis_id", "season")) |> 
  filter(position == "RB")

rb_data_00 <- rb_pbp_00 |>  group_by(season) |> 
  summarise(mean_height = weighted.mean(height, carries), 
            mean_weight = weighted.mean(weight, carries), 
            pos = last(position))


#### COMBINE MEAN DATASETS ####

mean_by_szn_qb <- rbind(qb_data_00, qb_data_10, qb_data)

mean_by_szn_wr <- rbind(wr_data_00, wr_data_10, wr_data)

mean_by_szn_te <- rbind(te_data_00, te_data_10, te_data)

mean_by_szn_rb <- rbind(rb_data_00, rb_data_10, rb_data)








