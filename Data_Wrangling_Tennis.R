
library(tidyverse)
library(tidymodels)

## I am deciding to use the 2015-2022 years. 
## The data has been extracted and cleaned using the functions in Tennis_functions.R.

# But, I want to do further wrangling and computations to enable the use of prospective models


# load in data (2015-2022) cleaned data
atp_matches_cleaned <- read.csv("C:/Users/timst/Documents/SMU/Fall_2022/STAT6341/Project/Tennis_Data_Reduced/reduced_clean_matches.csv",
                                header =T)

# currently I have the players but I want to put the each match on 1 line instead of 2


which(atp_matches_cleaned$server_won==1)
which(atp_matches_cleaned$server_won==0)

length(which(atp_matches_cleaned$server_won==1))
length(which(atp_matches_cleaned$server_won==0))

# make sure all the first 20152 are serverwon = 1 and the last 20152 are serverwon = 0
max(which(atp_matches_cleaned$server_won==1))
min(which(atp_matches_cleaned$server_won==0))


winners <- dplyr::filter(atp_matches_cleaned, atp_matches_cleaned$server_won==1)
losers <- dplyr::filter(atp_matches_cleaned, atp_matches_cleaned$server_won==0)


# Create vector that randomly picks winner or losers, so I can sample and combine later
sampler <- sample(c(0,1), length(winners$X), replace = T)

## Create variables
# First p1
p1 <- numeric(length(sampler))
p1.id <- numeric(length(sampler))
p1_svpt  <- numeric(length(sampler))
p1_1stIn  <- numeric(length(sampler))
p1_1stWon  <- numeric(length(sampler))
p1_2ndIn  <- numeric(length(sampler))
p1_2ndWon  <- numeric(length(sampler))
p1_df  <- numeric(length(sampler))
p1_ace  <- numeric(length(sampler))
p1_rpw_1st  <- numeric(length(sampler))
p1_rpw_2nd  <- numeric(length(sampler))
p1_pr_1stIn  <- numeric(length(sampler))
p1_pr_2ndin  <- numeric(length(sampler))
p1_pr_win_giv_1in  <- numeric(length(sampler))
p1_pr_win_giv_2in  <- numeric(length(sampler))
p1_pr_win_on_1st_serve  <- numeric(length(sampler))
p1_pr_win_on_2nd_serve  <- numeric(length(sampler))
p1_pr_win_on_serve  <- numeric(length(sampler))
p1_pr_win_two_first_serves  <- numeric(length(sampler))

#Then p2
p2 <- numeric(length(sampler))
p2.id <- numeric(length(sampler))
p2_svpt  <- numeric(length(sampler))
p2_1stIn  <- numeric(length(sampler))
p2_1stWon  <- numeric(length(sampler))
p2_2ndIn  <- numeric(length(sampler))
p2_2ndWon  <- numeric(length(sampler))
p2_df  <- numeric(length(sampler))
p2_ace  <- numeric(length(sampler))
p2_rpw_1st  <- numeric(length(sampler))
p2_rpw_2nd  <- numeric(length(sampler))
p2_pr_1stIn  <- numeric(length(sampler))
p2_pr_2ndin  <- numeric(length(sampler))
p2_pr_win_giv_1in  <- numeric(length(sampler))
p2_pr_win_giv_2in  <- numeric(length(sampler))
p2_pr_win_on_1st_serve  <- numeric(length(sampler))
p2_pr_win_on_2nd_serve  <- numeric(length(sampler))
p2_pr_win_on_serve  <- numeric(length(sampler))
p2_pr_win_two_first_serves  <- numeric(length(sampler))



## game numbers
set1GameNum  <- numeric(length(sampler))
set1OppGameNum  <- numeric(length(sampler))
set2GameNum  <- numeric(length(sampler))
set2OppGameNum  <- numeric(length(sampler))
set3GameNum  <- numeric(length(sampler))
set3OppGameNum  <- numeric(length(sampler))
set4GameNum  <- numeric(length(sampler))
set4OppGameNum  <- numeric(length(sampler))
set5GameNum  <- numeric(length(sampler))
set5OppGameNum  <- numeric(length(sampler))

p1_won <- numeric(length(sampler))



## Get info - Fill in results
for (i in 1:length(sampler))
{
  #pick winner  for p1
  if (sampler[i] == 1)
  {
    #assign p1
    p1[i] <- winners$server[i]
    p1.id[i] <- winners$server_id[i]
    p1_svpt[i] <- winners$s_svpt[i]
    p1_1stIn[i]  <- winners$s_1stIn[i]
    p1_1stWon[i]  <- winners$s_1stWon[i]
    p1_2ndIn[i]  <- winners$s_2ndIn[i]
    p1_2ndWon[i]  <- winners$s_2ndWon[i]
    p1_df[i]  <- winners$s_df[i]
    p1_ace[i]  <- winners$s_ace[i]
    p1_rpw_1st[i]  <- winners$s_rpw_1st[i]
    p1_rpw_2nd[i]  <- winners$s_rpw_2nd[i]
    p1_pr_1stIn[i]  <- winners$pr_1stin[i]
    p1_pr_2ndin[i]  <- winners$pr_2ndin[i]
    p1_pr_win_giv_1in[i]  <- winners$pr_w1_giv_1in[i]
    p1_pr_win_giv_2in[i]  <- winners$pr_w2_giv_2in[i]
    p1_pr_win_on_1st_serve[i]  <- winners$pr_win_on_1st_serve[i]
    p1_pr_win_on_2nd_serve[i]  <- winners$pr_win_on_2nd_serve[i]
    p1_pr_win_on_serve[i]  <- winners$pr_win_on_serve[i]
    p1_pr_win_two_first_serves[i]  <- winners$pr_win_two_first_serves[i]
    
    #assign p2
    p2[i] <- losers$server[i]
    p2.id[i] <- losers$server_id[i]
    p2_svpt[i] <- losers$s_svpt[i]
    p2_1stIn[i]  <- losers$s_1stIn[i]
    p2_1stWon[i]  <- losers$s_1stWon[i]
    p2_2ndIn[i]  <- losers$s_2ndIn[i]
    p2_2ndWon[i]  <- losers$s_2ndWon[i]
    p2_df[i]  <- losers$s_df[i]
    p2_ace[i]  <- losers$s_ace[i]
    p2_rpw_1st[i]  <- losers$s_rpw_1st[i]
    p2_rpw_2nd[i]  <- losers$s_rpw_2nd[i]
    p2_pr_1stIn[i]  <- losers$pr_1stin[i]
    p2_pr_2ndin[i]  <- losers$pr_2ndin[i]
    p2_pr_win_giv_1in[i]  <- losers$pr_w1_giv_1in[i]
    p2_pr_win_giv_2in[i]  <- losers$pr_w2_giv_2in[i]
    p2_pr_win_on_1st_serve[i]  <- losers$pr_win_on_1st_serve[i]
    p2_pr_win_on_2nd_serve[i]  <- losers$pr_win_on_2nd_serve[i]
    p2_pr_win_on_serve[i]  <- losers$pr_win_on_serve[i]
    p2_pr_win_two_first_serves[i]  <- losers$pr_win_two_first_serves[i]
    
    #assign games - Assign just how they are
    set1GameNum[i]  <- winners$set1GameNum[i]
    set1OppGameNum[i]  <- winners$set1OppGameNum[i]
    set2GameNum[i]  <- winners$set2GameNum[i]
    set2OppGameNum[i]  <- winners$set2OppGameNum[i]
    set3GameNum[i]  <- winners$set3GameNum[i]
    set3OppGameNum[i]  <- winners$set3OppGameNum[i]
    set4GameNum[i]  <- winners$set4GameNum[i]
    set4OppGameNum[i]  <- winners$set4OppGameNum[i]
    set5GameNum[i]  <- winners$set5GameNum[i]
    set5OppGameNum[i]  <- winners$set5OppGameNum[i]
    
    # response variable
    p1_won[i] <- winners$server_won[i]
    
  }
  
  # else statement means that sampler[i] ==0
  # have chosen loser for p1
  else
  {
    #assign p1
    p1[i] <- losers$server[i]
    p1.id[i] <- losers$server_id[i]
    p1_svpt[i] <- losers$s_svpt[i]
    p1_1stIn[i]  <- losers$s_1stIn[i]
    p1_1stWon[i]  <- losers$s_1stWon[i]
    p1_2ndIn[i]  <- losers$s_2ndIn[i]
    p1_2ndWon[i]  <- losers$s_2ndWon[i]
    p1_df[i]  <- losers$s_df[i]
    p1_ace[i]  <- losers$s_ace[i]
    p1_rpw_1st[i]  <- losers$s_rpw_1st[i]
    p1_rpw_2nd[i]  <- losers$s_rpw_2nd[i]
    p1_pr_1stIn[i]  <- losers$pr_1stin[i]
    p1_pr_2ndin[i]  <- losers$pr_2ndin[i]
    p1_pr_win_giv_1in[i]  <- losers$pr_w1_giv_1in[i]
    p1_pr_win_giv_2in[i]  <- losers$pr_w2_giv_2in[i]
    p1_pr_win_on_1st_serve[i]  <- losers$pr_win_on_1st_serve[i]
    p1_pr_win_on_2nd_serve[i]  <- losers$pr_win_on_2nd_serve[i]
    p1_pr_win_on_serve[i]  <- losers$pr_win_on_serve[i]
    p1_pr_win_two_first_serves[i]  <- losers$pr_win_two_first_serves[i]
    
    #assign p2 for winner
    p2[i] <- winners$server[i]
    p2.id[i] <- winners$server_id[i]
    p2_svpt[i] <- winners$s_svpt[i]
    p2_1stIn[i]  <- winners$s_1stIn[i]
    p2_1stWon[i]  <- winners$s_1stWon[i]
    p2_2ndIn[i]  <- winners$s_2ndIn[i]
    p2_2ndWon[i]  <- winners$s_2ndWon[i]
    p2_df[i]  <- winners$s_df[i]
    p2_ace[i]  <- winners$s_ace[i]
    p2_rpw_1st[i]  <- winners$s_rpw_1st[i]
    p2_rpw_2nd[i]  <- winners$s_rpw_2nd[i]
    p2_pr_1stIn[i]  <- winners$pr_1stin[i]
    p2_pr_2ndin[i]  <- winners$pr_2ndin[i]
    p2_pr_win_giv_1in[i]  <- winners$pr_w1_giv_1in[i]
    p2_pr_win_giv_2in[i]  <- winners$pr_w2_giv_2in[i]
    p2_pr_win_on_1st_serve[i]  <- winners$pr_win_on_1st_serve[i]
    p2_pr_win_on_2nd_serve[i]  <- winners$pr_win_on_2nd_serve[i]
    p2_pr_win_on_serve[i]  <- winners$pr_win_on_serve[i]
    p2_pr_win_two_first_serves[i]  <- winners$pr_win_two_first_serves[i]
    
    #assign games 
    # Notes winners$set1GameNum and losers$set1GameNum are the same.
    # TO get correct one, since he loser is the main player, switch these around.
    set1GameNum[i]  <- winners$set1OppGameNum[i]
    set1OppGameNum[i]  <- winners$set1GameNum[i]
    set2GameNum[i]  <- winners$set2OppGameNum[i]
    set2OppGameNum[i]  <- winners$set2GameNum[i]
    set3GameNum[i]  <- winners$set3OppGameNum[i]
    set3OppGameNum[i]  <- winners$set3GameNum[i]
    set4GameNum[i]  <- winners$set4OppGameNum[i]
    set4OppGameNum[i]  <- winners$set4GameNum[i]
    set5GameNum[i]  <- winners$set5OppGameNum[i]
    set5OppGameNum[i]  <- winners$set5GameNum[i]
    
    # response variable
    p1_won[i] <- losers$server_won[i]
  }
}

Tournament_name <- winners$tournament_name
Tournament_date <- winners$tournament_date
Tournament_level <- winners$tournament_level

Surface <- winners$surface
Year <- winners$year

## Now create data_frame
## finish these
atp_final <- data.frame(p1,p1.id,p2,p2.id, p1_svpt,p1_1stIn,p1_1stWon,p1_2ndIn,p1_2ndWon,
                        p1_df, p1_ace, p1_rpw_1st,p1_rpw_2nd,p1_pr_1stIn,p1_pr_2ndin,
                        p1_pr_win_giv_1in,p1_pr_win_giv_2in,p1_pr_win_on_1st_serve,p1_pr_win_on_2nd_serve,
                        p1_pr_win_on_serve,p1_pr_win_two_first_serves,
                        p2_svpt,p2_1stIn,p2_1stWon,p2_2ndIn,p2_2ndWon,
                        p2_df, p2_ace, p2_rpw_1st,p2_rpw_2nd,p2_pr_1stIn,p2_pr_2ndin,
                        p2_pr_win_giv_1in,p2_pr_win_giv_2in,p2_pr_win_on_1st_serve,p2_pr_win_on_2nd_serve,
                        p2_pr_win_on_serve,p2_pr_win_two_first_serves,
                        set1GameNum,set1OppGameNum,set2GameNum,set2OppGameNum,set3GameNum,set3OppGameNum,
                        set4GameNum,set4OppGameNum,set5GameNum,set5OppGameNum, Tournament_name, Tournament_date,
                        Tournament_level, Surface, Year,
                        p1_won)


#Write this to a file
write.csv(atp_final, "C:/Users/timst/Documents/SMU/Fall_2022/STAT6341/Project/Tennis_Data_Reduced/2015-2022_Final.csv")



#---------------------------------------------------------------------------------------------------------
## IGNORE: Work beforehand
# before I clean them, rename stuff for clarification
winners <- winners %>%
  rename(p1 = server, p2 = returner, p1_id = server_id, p2_id = returner_id,
         p1_svpt = s_svpt, p1_1stIn = s_1stIn, p1_1stWon = s_1stWon,
         p1_2ndIn = s_2ndIn, p1_2ndWon = s_2ndWon, p1_df = s_df, p1_ace = s_ace,
         p1_rpw_1st = s_rpw_1st, p1_rpw_2nd = s_rpw_2nd, p1_won = server_won,
         p1_pr_1stIn = pr_1stin, p1_pr_2ndIn = pr_2ndin, p1_pr_w1_giv_1in = pr_w1_giv_1in,
         p1_pr_w2_giv_2in = pr_w2_giv_2in, p1_pr_win_on_1st_serve = pr_win_on_1st_serve,
         p1_pr_win_on_2nd_serve = pr_win_on_2nd_serve, p1_pr_win_on_serve = pr_win_on_serve,
         p1_pr_win_two_first_serves = pr_win_two_first_serves)

losers <- losers %>%
  rename(p2 = server, p1 = returner, p2_id = server_id, p1_id = returner_id,
         p2_svpt = s_svpt, p2_1stIn = s_1stIn, p2_1stWon = s_1stWon,
         p2_2ndIn = s_2ndIn, p2_2ndWon = s_2ndWon, p2_df = s_df, p2_ace = s_ace,
         p2_rpw_1st = s_rpw_1st, p2_rpw_2nd = s_rpw_2nd, p2_won = server_won,
         p2_pr_1stIn = pr_1stin, p2_pr_2ndIn = pr_2ndin, p2_pr_w1_giv_1in = pr_w1_giv_1in,
         p2_pr_w2_giv_2in = pr_w2_giv_2in, p2_pr_win_on_1st_serve = pr_win_on_1st_serve,
         p2_pr_win_on_2nd_serve = pr_win_on_2nd_serve, p2_pr_win_on_serve = pr_win_on_serve,
         p2_pr_win_two_first_serves = pr_win_two_first_serves)





## remove duplicate columns before joining
winners <- winners %>% select(-c(set1GameNum,set1OppGameNum, set2GameNum,set2OppGameNum, set3GameNum,
                                 set3OppGameNum, set4GameNum,set4OppGameNum,set5GameNum,set5OppGameNum))

losers <- losers %>% select(-c(X,p2,p2_id, p1,p1_id, tournament_name,tournament_date,tournament_level,year,
                               score,surface))

#clean up some column names



atp_clean <- cbind(winners,losers)
## clean up game point names
atp_clean  <- atp_clean %>%
  rename(p1_set1GamesWon = set1GameNum, p2_set1GamesWon = set1OppGameNum, p1_set2GamesWon = set2GameNum,
         p2_set2GamesWon = set2OppGameNum, p1_set3GamesWon = set3GameNum, p2_set3GamesWon = set3OppGameNum,
         p1_set4GamesWon = set4GameNum, p2_set4GamesWon = set4OppGameNum, p1_set5GamesWon = set5GameNum,
         p2_set5GamesWon = set5OppGameNum)

# Need to make it so it isn't 
# this won't even work since 



