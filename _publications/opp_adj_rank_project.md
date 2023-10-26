---
title: "College Football Top 25 using Oppnent Adjusted EPA and Massey Ratings"
excerpt: "An in-depth explantation of college football oppenent adjustment EPA using ridge regression and using this adjusted EPA to calculate the CFB top 25 weekly, using the Massey rating system."
collection: portfolio
excerpt: 2023-09-01
---

## Introduction

As an avid college football enthusiast and a passionate fan, I eagerly await the release of the AP poll every Sunday during the season. It's a moment of great anticipation as I eagerly check to see where my team stands after their performance on Saturday and how other teams have fared. The thrill of Saturday games always leaves me wondering, 'Where will each team ascend to or descend in the rankings on Sunday?'

The AP poll is the sure-fire way to measure where each team stands in the race to both the College Football Playoffs and the National Championship. Now, the AP poll, the poll that means the most to almost all college football fans during the season, is determined by voters. This can often lead to an intense debate with a lot of disagreement among coaches, players, and fans. This curiosity led me to explore whether there are more technical methods for assessing college football team rankings. After doing research, I've arrived at a clear conclusion: indeed, there are.

In this analysis, I will use ridge regression to opponent adjust each team's offensive and defensive expected points added(EPA), and then use linear regression and passed data to predict the score of each game based off of this oppnent adjusted EPA. Finally, I will use the Massey Ratings with this predict score to calculate my Top 25. 

## Opponent Adjustment

### Setup 

To start, let's load in all the necessary packages we will need for this analysis. 

```
library(dplyr)
library(tidyverse)
library(cfbfastR)
library(glmnet)
```

Next, we can gather the data, which will be the play-by-play data from the cfbfastR package. 

```
pbp <- cfbfastR::load_cfb_pbp(seasons = 2023) 
```

I need to do a bit of cleaning to this data. First, let's ensure we only have FBS teams in the data set. Since FBS teams do sometimes play non FBS schools, we can trim the dataset by fitering for at least one school being FBS. We must group the data by each game. Then, let's pull out the necessary variables for the analysis using the `select()` function. We will need both teams on offense and defesne, the game id, EPA, and home field advantage(hfa). This is a binary variable, so 1 if the team is at home and 0 if the team is away. For the analysis to work, let's change the hfa variable to a character, since we will later include it in the dummy variables. 

```
keep.conf <- c("SEC", "Moutain West", "American Athletic", "Pac-12", "Mid-American", "Sun Belt", "ACC", "Big 12", "Big Ten", 
               "FBS Independents", "Conference USA")
pbp.clean <- pbp |> filter(!is.na(EPA)) |> group_by(game_id) |> 
filter(offense_conference %in% keep.conf | defense_conference %in% keep.conf) |> 
  mutate(hfa = ifelse(pos_team == home, 1, 0)) |> 
  select(pos_team, def_pos_team, game = game_id, EPA, hfa) |> 
  ungroup()
  
pbp.clean$hfa <- as.character(pbp.clean$hfa)
```

The next step in performing this regression is to create dummy variables for all offenes and deffenses in the data set. Let's also include hfa in this dummy data set. We then can pull out only the `EPA` column from the `pbp.clean` data and bind it with the dummy data, since this is out target variable. (Note there will be an Intercept variable when using the model.matrix function, so we will drop this from out data frame). 

```
EPA.data <- pbp.clean |> select(EPA)

dummies <- model.matrix(~hfa+pos_team+def_pos_team, data = pbp.clean)

data.dummies <- as.data.frame(dummies)

data.dummies <- cbind(data.dummies, EPA.data) |> select(-`(Intercept)`)

```

Now, let's seperate the inputs(x), offense, defense, and hfa, and the output(y), which is EPA. The input data should be a matrix for the ridge regression function in R. 

```
x <- as.matrix(data.dummies[, -ncol(data.dummies)])
y <- data.dummies$EPA
```
### Model

Ridge regression requires tuning hyperparameter lambda. We want the lambda that minimizes the MSE the most, so I found lambda to be around 175 in most cases for a full season of data. 

Once we do this, it is time to run the model. We can use `glmnet()` to run the model, making sure `alpha = 0` to make sure we run ridge regression. Let's also pull out all the coefficients from the model, since this is the variable we want. 

```
ridge_model <- glmnet(x, y, alpha = 0, lambda = 175)
ridge.coeff <- coef(ridge_model, s = 175)  
```

Then we can pull out the intercept term and add it to each offense and defense's EPA coefficient. In this step we can grab offense and defense seperatly by using the "pos_team" and "def_team" characters from the front of the school name. (These were added when making the dummy data set). 

```
intercept <- ridge.coeff[1]

pos_team_coeffs <- ridge.coeff[grep("^pos_team", rownames(ridge.coeff)), , drop = FALSE] + intercept
def_pos_team_coeffs <- ridge.coeff[grep("^def_pos_team", rownames(ridge.coeff)), , drop = FALSE] + intercept

```

Next, we will make some adjustments to each to make the coeffients an actual data set. First, we must make the two matrices a data fram. Within this we can move the teams from the row names and make them actual variables called `team`, and we can make the adjusted EPA another column. After this, we must get rid of the lingering rownames from transforming the matrix and then remove the "pos_team" or "def_team" from the from of each school name. 

```
offense <- data.frame(
  team = rownames(pos_team_coeffs),
  adjmodelOff = pos_team_coeffs[, 1]
)
rownames(offense) <- NULL

offense$team <- gsub("^pos_team", "", offense$team)

defense <- data.frame(
  team = rownames(def_pos_team_coeffs),
  adjmodelDef = def_pos_team_coeffs[, 1]
)
rownames(defense) <- NULL

defense$team <- gsub("^def_pos_team", "", defense$team)
```

The last step of this regression is to clean up both the offense and defense data we have created and combine them into one big data set. We can also add in the original, raw EPA to compare to our new opponent adjusted EPA. Lastly, we can do one final filter to ensure we only have FBS teams left in the set. We can do this by loading in the cfb team data set (an API key is needed) and filtering for the `keep.conf` list made earier. 

```
off_epa_game <- pbp|> filter(!is.na(EPA)) |> group_by(game_id) |> 
  mutate(hfa = ifelse(pos_team == home, 1, 0)) |> 
  group_by(game_id, pos_team, def_pos_team) |> summarise(rawOffEPA = mean(EPA)) |> 
  left_join(defense, by = c("def_pos_team" = "team")) |> 
  mutate(adjOffEPA = rawOffEPA - adjmodelDef) |> select(game = game_id, player = pos_team,
                                                        rawOffEPA, adjOffEPA) |> ungroup()

def_epa_game <- pbp|> filter(!is.na(EPA)) |> group_by(game_id) |> 
  mutate(hfa = ifelse(pos_team == home, 1, 0)) |> 
  group_by(game_id, def_pos_team, pos_team) |> summarise(rawDefEPA = mean(EPA)) |> 
  left_join(offense, by = c("pos_team" = "team")) |> 
  mutate(adjDefEPA = rawDefEPA - adjmodelOff) |> select(game = game_id, player = def_pos_team,
                                                        rawDefEPA, adjDefEPA) |> ungroup()

opp.adj <- off_epa_game |> left_join(def_epa_game, by = c("game", "player"))

teams <- load_cfb_teams() |> select(school, conference)

opp.adj <- opp.adj |> left_join(teams, by = c("player" = "school")) |> 
  filter(conference %in% keep.conf)
```

Now we are all finished with the opponent adjustment. Let's move on to the Massey Ratings!

## Linear Model 

### Setup 

We first need training data for the model. To do this, follow the steps from the ridge regression opponent adjuste for all the years prior to make the data. For this analysis I am using data from 2016 to 2022 to build the set. I loaded in the data from 2016-2019 and ran the regression steps, and did the same from 2020-2022. We then bind them to create the training data. 

```
train <- rbind(opp.adj16_19, opp.adj20_22)
```

Since we are trying to predict how many points teams score, we need to gather that data and combine it with the training data. To do this load in all the seasons of game data from cfbfastR and then bind them. After that we can pull out the variables needed. 

```
game1 <- cfbfastR::cfbd_game_info(2016)
game2 <- cfbfastR::cfbd_game_info(2017)
game3 <- cfbfastR::cfbd_game_info(2018)
game4 <- cfbfastR::cfbd_game_info(2019)
game5 <- cfbfastR::cfbd_game_info(2020)
game6 <- cfbfastR::cfbd_game_info(2021)
game7 <- cfbfastR::cfbd_game_info(2022)

game <- rbind(game1, game2, game3, game4, game5, game6, game7, game8)

home <- game |> 
  select(player = home_team, season, game = game_id, score = home_points) 

away <- game |>
  select(player = away_team, season, game = game_id, score = away_points) 

all.games <- rbind(home, away)
```


Lastly, with the two data sets we created, we can make the training data. 

```
model.dat <- all.games |> left_join(train, by = c("game", "player"))
model.dat <- na.omit(model.dat)
```

Now we are all set to model! 

### Model 

To create the model, use the `lm()` function in R. Out target variable is `Score` and the input variables are the adjusted EPA for both offense and defense for each team. You can view it using hte `summary()` function. 

```
epa.mod <- lm(score ~ adjOffEPA + adjDefEPA, data = model.dat)
summary(epa.mod)
```

Now that the model has been run, we can make score "predictions" based on our adjustment for the 2023 games that have already been played and add it to the data. 

```
opp.adj$predScore <- predict(epa.mod, opp.adj)
```

Now it is time to do the ratings! 

## Massey Ratings 

The Massey Method and Rating system rates teams based on the theory of least squares and uses the difference in points score by one team against another team. It is a commonly used rating in sports. 

To do this in R, it is simple! There is a function that does the work in the comperes package. First, though, we must get the data in the correct format. We can do this by selecting all the variables we need and then using the `to_pairgames()` function to get the correct format. This function just changes the dataset to have player 1, player 2, score 1 and score 2 all as variables. 

```
dat <- opp.adj |>
  ungroup() |> select(game, player, predScore)

dat <- comperes::to_pairgames(dat)
```

Finally, we use the `rank_massey()` function to rate teams and rank them for my CFB Top 25. 

```
rank.adj.epa <- comperank::rank_massey(dat,
                                           keep_rating = TRUE) |> 
 arrange(desc(rating_massey))

show(rank.adj.epa)

rank.data <- as.data.frame(rank.adj.epa)
```

## Conclusion 

In conclusion, we can get a pretty good CFB ranking using adjusted EPA and the Massey Rating Method. There is no perfect way to rate and rank teams, but this was just a fun project to do and see the result. Thank you for following my analysis and I hope you learned something new! 






