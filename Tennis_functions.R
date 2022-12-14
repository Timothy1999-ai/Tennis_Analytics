
library(tidyverse)
library(stringr)
library(plotly)
library(scales)

#--> Load libraries
library(dplyr)
library(lubridate)
library(tidymodels)


save_atp_match_data <- function(year, save_path)
{
  
  # -- link to Sackmann's data
  atp_root <- 'https://raw.githubusercontent.com/JeffSackmann/tennis_atp/master/atp_matches_'
  data_path <- paste(atp_root, year, '.csv', sep = "")
  
  # -- Read in data
  full_data <- read.csv(data_path)
  
  
  # Okay, now get this data organized into server and returner
  winner_frame <- data.frame(server =  full_data$winner_name,
                             server_id = full_data$winner_id,
                             returner = full_data$loser_name,
                             returner_id = full_data$loser_id,
                             s_svpt =  full_data$w_svpt,
                             s_1stIn =  full_data$w_1stIn,
                             s_1stWon = full_data$w_1stWon,
                             s_2ndIn = full_data$w_svpt - full_data$w_1stIn - full_data$w_df,
                             s_2ndWon =  full_data$w_2ndWon,
                             s_df = full_data$w_df,
                             s_ace = full_data$w_ace,
                             s_rpw_1st = full_data$l_1stIn - full_data$l_1stWon,
                             s_rpw_2nd = full_data$l_svpt - full_data$l_1stIn - full_data$l_2ndWon,
                             server_won = 1,
                             tournament_name = full_data$tourney_name,
                             tournament_date = full_data$tourney_date,
                             tournament_level = full_data$tourney_level,
                             year = year,
                             score = full_data$score,
                             surface = full_data$surface,
                             stringsAsFactors = FALSE
  )
  
  loser_frame <- data.frame(server =  full_data$loser_name,
                            server_id = full_data$loser_id,
                            returner = full_data$winner_name,
                            returner_id = full_data$winner_id,
                            s_svpt =  full_data$l_svpt,
                            s_1stIn =  full_data$l_1stIn,
                            s_1stWon = full_data$l_1stWon,
                            s_2ndIn = full_data$l_svpt - full_data$l_1stIn - full_data$l_df,
                            s_2ndWon =  full_data$l_2ndWon,
                            s_df = full_data$l_df,
                            s_ace = full_data$l_ace,
                            s_rpw_1st = full_data$w_1stIn - full_data$w_1stWon,
                            s_rpw_2nd = full_data$w_svpt - full_data$w_1stIn - full_data$w_2ndWon,
                            server_won = 0,
                            tournament_name = full_data$tourney_name,
                            tournament_date = full_data$tourney_date,
                            tournament_level = full_data$tourney_level,
                            year = year,
                            score = full_data$score,
                            surface = full_data$surface,
                            stringsAsFactors = FALSE
  )
  
  # -- Combine winner and loser server data into a single dataframe
  atp_dat <- rbind(winner_frame, loser_frame)  
  
  print(paste('Year is: ', year, sep = ''))
  print(paste('Warning, number of incomplete entries is: ', sum(!complete.cases(atp_dat)), sep = '' ))
  
  atp_dat <- atp_dat %>%
    filter(complete.cases(atp_dat))
  
  print(paste('Keeping ', nrow(atp_dat), ' rows!', sep = ''))
  
  
  ## create  some new variables
  atp_dat_full <- atp_dat %>%
    
    mutate(
      # --> Pr(1st serve in)
      pr_1stin = s_1stIn/s_svpt,
      
      # --> Pr(2nd serve in)
      pr_2ndin = s_2ndIn/(s_2ndIn + s_df),
      
      # --> Pr(Win service point | 1st serve in)
      pr_w1_giv_1in = s_1stWon/s_1stIn,
      
      # --> Pr(Win service point | 2nd serve in)
      pr_w2_giv_2in = s_2ndWon/s_2ndIn,
      
      # --> Pr(win on 1st serve)
      pr_win_on_1st_serve = pr_w1_giv_1in * pr_1stin,
      
      # --> Pr(win on 2nd serve)
      pr_win_on_2nd_serve = s_2ndWon/ (s_svpt - s_1stIn),
      
      #-->Pr(win on serve)
      pr_win_on_serve = pr_1stin*pr_w1_giv_1in + ((1- pr_1stin)*pr_2ndin*pr_w2_giv_2in) ,
      
      # --> Pr(win w/ 2 first serves)
      
      pr_win_two_first_serves = pr_1stin*pr_w1_giv_1in + ((1- pr_1stin)* pr_1stin*pr_w1_giv_1in )
    )
  
  
  
  filename = paste(save_path, 'atp_', year, '.csv', sep = '')
  
  write.csv(x = atp_dat_full, row.names = FALSE,
            file = filename)
  
  
}

# --- Save ATP DAta

saving_path <- "C:/Users/timst/Documents/SMU/Fall_2022/STAT6341/Project/Tennis_Data/"

saving_path2 <- "C:/Users/timst/Documents/SMU/Fall_2022/STAT6341/Project/Tennis_Data_Reduced/"


for(year in 2015:2022)
{
  save_atp_match_data(year = year,save_path = saving_path2)
}

# -- DONE



### Combine ATP match data into one dataset

library(dplyr)
library(readr)
total_matches <- list.files(path="C:/Users/timst/Documents/SMU/Fall_2022/STAT6341/Project/Tennis_Data_Reduced/", full.names = TRUE) %>% 
  lapply(read_csv) %>% 
  bind_rows 

## write to .csv
write.csv(x = total_matches, row.names = F, 
          file = "C:/Users/timst/Documents/SMU/Fall_2022/STAT6341/Project/Tennis_Data_Reduced/atp_reduced_matches.csv")


# Cleaning
#------------------------------------------------------------------------------
## Fix tennis score
library(tidyverse)
atp_total_matches <- read.csv("C:/Users/timst/Documents/SMU/Fall_2022/STAT6341/Project/Tennis_Data_Reduced/atp_reduced_matches.csv",
                              header = T)

atp_total_matches <- read.csv("C:/Users/timst/Documents/SMU/Fall_2022/STAT6341/Project/Tennis_Data/atp_2022.csv")

## Grab 2015 to 2022


numScores <- str_extract_all(atp_total_matches$score, "\\(?[0-9,.]+\\)?")


## Some have parentheses so it will call that NA. But the parentheses was just extra info and not info concenrning match data
numScores <- lapply(numScores, as.numeric)

#Remove all NA.

numScores <- lapply(numScores, na.omit)

# Count how many sets there were in each match

setLength <- function (match)
{
  length(match)/2
}
setCount <- lapply(numScores, setLength)

setCountVec <- unlist(setCount)

# Create functions to get Game Counts for each player and each set
set1Games <- function (match)
{
  ifelse (length(match) >= 1, match[1], 0)

}

set1OppGames <- function (match)
{
  ifelse(length(match) >= 2, match[2],0)
}

set2Games <- function (match)
{
  ifelse (length(match) >= 3, match[3], 0)
  
}

set2OppGames <- function (match)
{
  ifelse(length(match) >= 4, match[4],0)
}

set3Games <- function (match)
{
  ifelse (length(match) >= 5, match[5], 0)
  
}

set3OppGames <- function (match)
{
  ifelse(length(match) >= 6, match[6],0)
}

set4Games <- function (match)
{
  ifelse (length(match) >= 7, match[7], 0)
  
}

set4OppGames <- function (match)
{
  ifelse(length(match) >= 8, match[8],0)
}

set5Games <- function (match)
{
  ifelse (length(match) >= 9, match[9], 0)
  
}

set5OppGames <- function (match)
{
  ifelse(length(match) >= 10, match[10],0)
}


## Apply these functions to 
set1GameNum <- lapply(numScores, set1Games)
set1OppGameNum  <- lapply(numScores, set1OppGames)

set2GameNum <- lapply(numScores, set2Games)
set2OppGameNum  <- lapply(numScores, set2OppGames)

set3GameNum <- lapply(numScores, set3Games)
set3OppGameNum  <- lapply(numScores, set3OppGames)

set4GameNum <- lapply(numScores, set4Games)
set4OppGameNum  <- lapply(numScores, set4OppGames)

set5GameNum <- lapply(numScores, set5Games)
set5OppGameNum  <- lapply(numScores, set5OppGames)


## unlist all the variables to get them as vectors
set1GameNum <- unlist(set1GameNum)
set1OppGameNum  <- unlist(set1OppGameNum)

set2GameNum <- unlist(set2GameNum)
set2OppGameNum  <- unlist(set2OppGameNum)

set3GameNum <- unlist(set3GameNum)
set3OppGameNum  <- unlist(set3OppGameNum)

set4GameNum <- unlist(set4GameNum)
set4OppGameNum  <- unlist(set4OppGameNum)

set5GameNum <- unlist(set5GameNum)
set5OppGameNum  <- unlist(set5OppGameNum)

atp_total_matches <- data.frame(atp_total_matches, set1GameNum, set1OppGameNum, set2GameNum, set2OppGameNum,
                                set3GameNum, set3OppGameNum, set4GameNum,  set4OppGameNum, set5GameNum, set5OppGameNum)

#

# save the data
write.csv(atp_total_matches, "C:/Users/timst/Documents/SMU/Fall_2022/STAT6341/Project/Tennis_Data_Reduced/2022_clean_matches.csv")


## just use response variable serverwon

## --------------------------------------------------------------------------------
# build logistic model
# make sure that is a factor
# Note, every there are the same amount of serverWon =1 and serverWon = 0.
atp_total_matches$server_won <- as.factor(atp_total_matches$server_won)

predictors.index <- c(1,3,5:15,17,18,20,21:38)


#########################
##Base model - Logistic##
#########################

##Data split
set.seed(123)
tennis_split  <- initial_split(atp_total_matches, prop = 0.8, strata = "server_won")
tennis_train  <- training(tennis_split)
tennis_test   <- testing(tennis_split)

##Simple Recipe
tennis_rec <- recipe(server_won ~ ., data = tennis_train[,predictors.index]) %>% 
  step_center(all_predictors(), -all_nominal()) %>% 
  step_scale(all_predictors(), -all_nominal()) %>% 
  step_dummy(all_nominal(), -all_outcomes(), one_hot = FALSE)

tennis_prep <- prep(tennis_rec)

##Resampling
set.seed(123)
tidy_kfolds <- vfold_cv(tennis_train, v = 10, repeats = 5, strata = "server_won")

tennis_logistic <- logistic_reg() %>% 
  set_mode("classification") %>%
  set_engine("glm") 

#Specify modeling procedure
tennis_wf1 <- workflow() %>% 
  add_recipe(tennis_rec) %>% 
  add_model(tennis_logistic)

tennis_fit <- fit_resamples(
  tennis_wf1,
  resamples = tidy_kfolds,
  metrics = metric_set(accuracy, kap, roc_auc))

##Re-sampled model performance
tennis_fit %>% collect_metrics()

##Point estimates
pitch_glm_fit <- fit(tennis_wf1, data=tennis_train)

##Test performance
tennis_wf1 %>%
  last_fit(tennis_split, metrics = metric_set(accuracy, kap, roc_auc)) %>% 
  collect_metrics()
