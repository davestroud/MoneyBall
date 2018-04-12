library(tidyverse)
library(ggplot2)
library(gridExtra)
library(caTools)
library(caret)
library(corrplot)
library(ROCR)
library(memisc)
library(rpart)
library(rpart.plot)
library(MASS)
library(gbm)

# Baseball Zip File Url
url <- 'http://seanlahman.com/files/database/lahman-csv_2015-01-24.zip'
# create a temp file
temp <- tempfile()
# fetch the file into the temp. file and download it
download.file(url, temp)
# extract the target file from temp. file
teams <- read.csv(unz(temp, 'Teams.csv'))
salaries <- read.csv(unz(temp, 'Salaries.csv'))


teams$teamID[teams$teamID == 'CAL'] <- 'LAA'
teams$teamID[teams$teamID == 'FLO'] <- 'MIA'
teams$teamID[teams$teamID == 'ML4'] <- 'MIL'
teams$teamID[teams$teamID == 'MON'] <- 'WAS'
teams$teamID[teams$teamID == 'ANA'] <- 'LAA'






