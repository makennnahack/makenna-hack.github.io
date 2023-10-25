---
title: "Height Analysis in the NFL by Offensive Positon"
excerpt: "An in-depth analysis of height trends of offensive skill positons in the NFL"
collection: portfolio
---

## Introduction 

As an avid football enthusiast and a dedicated follower of the NFL, I've developed a deep appreciation for the ever-evolving nature of both the league and the game of football itself. With each passing season, I've observed how football undergoes continuous transformation, from the skill positions players occupy on the field to the intricacies of play calling and the strategies employed by coaches. This dynamic evolution keeps the sport fresh, exciting, and endlessly intriguing.

One of the most conspicuous yet exceptionally captivating changes I've noticed throughout the league pertains to the shifting trends in player heights within different positions. It's an aspect of the change that often goes under the radar but has a profound impact on player roles, and overall gameplay. The NFL has seen an intriguing change in the heights of players occupying various offensive positions.

In this post, I provide a comprehensive exploration of the height trends in the NFL over the years, focusing on different offensive positions, I will use the average height per year. It is not that simple, though, as some players play less than others. I will dive into this further in the post.

By delving into the historical data and analyzing the trends, we can gain valuable insights into how the game has evolved and how these shifts in player dimensions have contributed to the NFL's ever-changing landscape.

Positions up for analysis: Quarterback(QB), Wide Receiver(WR), Tight End(TE), and Running Back(RB). 

## Setup

To begin, let's load load in the necessary packages. 

```
library(tidyverse)
library(nflfastR)
library(nflverse)
```

The data I will be using for this analysis will the rosters data set for every position. To get the weights for each position, we will use play-by-play data for quarterback play counts, and player stats data set for wide receivers, tight ends, and running backs. 

First, let's load in the roster's data, which will be consistent through all years and through all positions. I want to perform this analysis through 3 decades, 2000-2009, 2010-2019, and 2020-2023. 

```
rosters <- load_rosters(2000:2023)
```

After loading in the rosters data set, it makes sense to clean the variables a bit. Let's keep all that is necessary for this project. gsis_id, which is the player's unique id, I will use this for merging data, season, full_name, position, and, of course, height. 

```
rosters_clean <- rosters |> select(gsis_id, season, full_name, position, height)
```

Now that we have the rosters and necessary variables, let's build the different functions to gather the average height data for each position. 

## Functions 

### Quarterbacks


To build the function to gather the average heights for quarterbacks among each year, let's start by creating a function with minimum and maximum years as inputs.

```
qb_function <- function(min_year, max_year) {}
```

To get the end data needed, let's start by loading in the play-by-play data for the specified years within the function. After this, I can merge the play-by-play with the cleaned rosters data by id and gsis_id, as well as season. This will allow me to get every individual player's play-by-play data, and then filter so I am only left with QBs. 

```
pbp_clean <- pbp |> 
  left_join(rosters_clean, by = c("id" = "gsis_id", "season")) |> 
  filter(position == "QB")
```

Once this is done, let's used the cleaned pbp and the "summarise()" function to pull out the player's name, how many plays the quarterback had, and their height and position. To ensure I get each individual quarterback by each season, I used the "group_by()" function with variables "id", and "season".

```
qb_pbp <- pbp_clean |> group_by(id, season) |> 
  summarise(name = last(name), 
            plays = sum(pass[!is.na(pass)]) + sum(rush[!is.na(rush)]), 
            height = last(height), weight = last(weight), position = last(position))
```

Finally, let's build the final data set by first grouping the data by season and making sure all height NA's are thrown out of the data set. Once this is done, use the "weight_mean()" function inside the summarise function to get the average height for quarterbacks weight by the number of plays the QB had that season. 

```
qb_data <- qb_pbp |>  group_by(season) |> filter(!is.na(height), !is.na(weight)) |>
  summarise(mean_height = weighted.mean(height, plays), 
            mean_weight = weighted.mean(weight, plays), 
            pos = last(position))
```

The final function will look like this: 

```
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

return(qb_data)
}
```

And building the whole data set will look something like this: 

```
qb_data_00 <- qb_function(2000, 2009)
qb_data_10 <- qb_function(2010, 2019)
qb_data_20 <- qb_function(2020, 2023)

qb_data <- rbind(qb_data_00, qb_data_10, qb_data_20)
```

The "rbind()" function will bind each decade data set by each row, so we have all the data in one place. 

Here is a glimpse of the data set I have just built. This is the first 2000s decade from 2000 to 2009. 

![](/makenna-hack.github.io/portfolio/off_heights_post/qb_final_data.png)

### Wide Receivers and Tight Ends

Now, we can do something similar for both Wide Receivers and Tight Ends. First, let's build the function again. 

```
rec_function <- function(min_year, max_year, pos) {}
```

Instead of using the play-by-play data to get total number of plays for the weight, let's use the player stats function built into nflfastR. Then, let's clean the data a bit to pull out the WR or TE's total receptions per season. 

```
player_stats <- load_player_stats(seasons = min_year:max_year)

clean_player_stats <- player_stats |> 
  group_by(player_id, season) |> 
  summarise(player_id = last(player_id), 
            name = last(player_name), 
            rec = sum(receptions))
```

After this, let's join the clean player stats with the clean rosters data created earlier. 

```
merged <- clean_player_stats |> 
  left_join(rosters_clean, by = c("player_id" = "gsis_id", "season")) |> 
  filter(position == pos)
```

Now, let's used the merged data to calculate the mean height by year, weighted by the number of receptions these ball-catchers have. 

```
data <- merged |>  group_by(season) |> 
  summarise(mean_height = weighted.mean(height, rec), 
            pos = last(position))
```

Putting the whole function together, we get: 

```
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
            pos = last(position))

return(data)
}
```

Now, let's cacluate run the function and calculate the weighted mean height for each season within each decade. 

```
wr_data_00 <- rec_function(2000, 2009, "WR")
wr_data_10 <- rec_function(2010, 2019, "WR")
wr_data_20 <- rec_function(2020, 2023, "WR")

wr_data <- rbind(wr_data_00, wr_data_10, wr_data_20)
```
```
te_data_00 <- rec_function(2000, 2009, "TE")
te_data_10 <- rec_function(2010, 2019, "TE")
te_data_20 <- rec_function(2020, 2023, "TE")

te_data <- rbind(te_data_00, te_data_10, te_data_20)
```

Now we have two more data sets that look similar to the previously made qb_data set from the Quarterback section. 

### Running Backs

We are almost finished building the necessary data. For running backs, let's do something similar to the WRs and TEs. Using the same player stats data set, but clean it to pull out all the carries each back has had by season. Then, let's do what we always do with the rosters and merge with the cleaned RB data. 

```
clean_player_stats <- player_stats |> 
    group_by(player_id, season) |> 
    summarise(player_id = last(player_id), 
              name = last(player_name), 
              carries = sum(carries))

merged <- clean_player_stats |> 
    left_join(rosters_clean, by = c("player_id" = "gsis_id", "season")) |> 
    filter(position == pos)
```

After that, let's calcuate the weighted mean! 

```
data <- merged |>  group_by(season) |> 
    summarise(mean_height = weighted.mean(height, carries), 
              mean_weight = weighted.mean(weight, carries), 
              pos = last(position))
```

The final function will look like this: 

```
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
              pos = last(position))
              
  return(data)
}
```

Of course, let's run the function and make the final required data set.  

```
rb_data_00 <- run_function(2000, 2009, "RB")
rb_data_10 <- run_function(2010, 2019, "RB")
rb_data_20 <- run_function(2020, 2023, "RB")

rb_data <- rbind(rb_data_00, rb_data_10, rb_data_20)
```

### All Positions by Year 

Let's also calculate the weighted mean height for each individual season. We can do this using the same function used for the mean height by year function. Instead of returning the "data" data set, let's return the "merged" data set. This will give us a data set with each individual player's height, what season it was, and how many plays, receptions, or carries the player had. Let's call this "postion"_data_dec. 

After this, let's add a decade variable. This will allow for a nice visualization later. 

```
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
```

Now, let's calculate the mean by decade(for a visualization later) and ungroup the data, so we can use another function later. 

```
qb_data_dec <- qb_data_dec |> dplyr::filter(!is.na(height)) |> ungroup()
qb_mean_by_dec <- qb_data_dec |> group_by(decade) |> summarise(mean_dec = mean(height))

wr_data_dec <- wr_data_dec |> dplyr::filter(!is.na(height)) |> ungroup()
wr_mean_by_dec <- wr_data_dec |> group_by(decade) |> summarise(mean_dec = mean(height))

te_data_dec <- te_data_dec |> dplyr::filter(!is.na(height)) |> ungroup()
te_mean_by_dec <- te_data_dec |> group_by(decade) |> summarise(mean_dec = mean(height))

rb_data_dec <- rb_data_dec |> dplyr::filter(!is.na(height)) |> ungroup()
rb_mean_by_dec <- rb_data_dec |> group_by(decade) |> summarise(mean_dec = mean(height))
```

Let's use this function to calculate the percentage of plays, receptions, or carries, based on the different positions (QB, WR, TE, RB), by each height that exists in the data set. 

```
decade_heights <- function(data, var, by_vars, weights = NULL) {
  data |> 
    count({{ by_vars }}, {{ var }}, wt = {{ weights }}) |> 
    mutate(
      .by = {{ by_vars }},
      pct = n / sum(n))
}
```

Note the curly brackets are apart of Tidy Evaluation and tells the function to use the value stored inside the argument, rather than the argument as a literal variable name. 

Next, let's run this for each position, giving us our final data set. 

```
qb_heights_decades <- decade_heights(qb_data_dec, height, by_vars = decade, weights = plays)
wr_heights_decades <- decade_heights(wr_data_dec, height, by_vars = decade, weights = rec)
te_heights_decades <- decade_heights(te_data_dec, height, by_vars = decade, weights = rec)
rb_heights_decades <- decade_heights(rb_data_dec, height, by_vars = decade, weights = carries)
```

Now that all the necessary data is calculated and put together, let's visualize! 

## Visualization

To create plots for each different position group, to see the weight mean by season, let's use a function. 

```
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
      plot.background = element_rect(fill = "white")
    )
}
```

Using ggplot and the data sets that were made in the previous steps, a quick function will do the trick to visualize this data. The inputs being the data set and a string indicating which position group we are visualizing. Using the "paste0()" function allows for the plot title to include the position group. Lastly, I added my own theme to the plot. 

Let's run the function for each position group. 

```
qb_plot <- graph(qb_data, "Quarterbacks'")
wr_plot <- graph(wr_data, "Wide Receivers'")
te_plot <- graph(te_data, "Tight Ends'")
rb_plot <- graph(rb_data, "Running Backs'")
```
Next, let's visualize the distribution of heights by each decade. Let's use the last couple data sets created, using the percent of either plays, receptions, or carries (per height) on the X axis, and height on the Y axis. The "facet_wrap" function will essentially group the data by the decade variable created in the earlier section. Here is the function on how to do this.  

```
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
      plot.background = element_rect(fill = "white")
    )
}
```
## Summary 

Finally, the analysis we have been waiting for! 

Let's start with looking at the season by season means for each position. 

![](/makenna-hack.github.io/portfolio/off_heights_post/mean_plot.jpg)

Looking at these plots, we can conclude many things. Starting with the quarterback position, we can see the average height for a QB increased heavily going into 2009. It was a pretty fast increase. From then to about 2016, there was somewhat of a stead increase. This recent decrease in average height could be due to the fact that teams now want more of a threat at QB. This means that a mobile quarterback is more valuable. Someone who can scramble when needed and maybe pick up a few yards or a first down, rather than a guy who lingers in the pocket. Good runners tend to not be as tall, meaning this could be a potential reason for a decreasing average. 

Moving on to the wide receivers, it is obvious there is a very inconsistent average height from season to season. The mean heights seem to alternate, being high one season and lower the next. This could mean that there is not one height to aim for if someone is trying to be a successful WR, but rather a wide range of heights. 

Next, the tight end position has been on a steady increase since 2000, but we see it drop heavily in 2021. Now, in 2021, the average height is still a good bit higher than past decades. A potential explanation for this is that the TE position now-a-days wants players who can catch the ball, muscle their way through the defense, and also block for the WRs, RBs, or even QB on the offense. This group is more of a hybrid between an O-lineman and a WR. 

Lastly, looking at running backs, we can see not only is this the shortest skill position on offense, but the mean height has been steadily decreasing since the start of the century. A few spikes to note here and there, but essentially RBs are getting shorter. This could be due to two reasons. First, as mentioned in the QB summary, shorter rushers tend to be more efficient. They are also harder to tackle. Two, a shorter back is easier to hide in an offensive formation. Teams can hide their RB behind linemen to divert the attention from the ball carrier.

![](/makenna-hack.github.io/portfolio/off_heights_post/dist_plot.jpg)

Now, looking at the plot of each distribution in the three decades, the mean pattern is clear. WR and RB average height has not change much decade to decade. TE average height has clearly gotten larger and on the other side QB height has increased from the 2000s to the 2010s, and has since inched back, and even under, the mean from the 2000s. These trends were all pretty aparent when looking at the season to season plots. 

## Conclusion

In conclusion, it is fun to look at changes in The National Football League! Something as simple as average height changes per year can tell us a lot about each position. Analyzing height can lead to further questions, such as what are these changes due to, and allow for more technical or complicated findings. I hope this was a good lesson in pulling data from nflverse, aggregating the data, making calculations, and then crafting nice visualizations. I also hope, after following along, you now have questions about the NFL that can be answered with data! 

Thank you for reading! 









