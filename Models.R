# The purpose of this script is to be able to reorganize the data so models can be ran
library(tidyverse)
library(tidymodels)
library(lubridate)

# 1) Read in data that has been scraped and cleaned

atp_final <- read.csv("C:/Users/timst/Documents/SMU/Fall_2022/STAT6341/Project/Tennis_Data_Reduced/2015-2022_Final.csv",
                                                       header =T)


