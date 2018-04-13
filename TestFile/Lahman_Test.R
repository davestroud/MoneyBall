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

# new column to display if team made playoffs
# already column is team won division but need to add wild card teams
teams$Playoff <- teams$DivWin
# this only adds the the division winners so we need to also add the wild card winners
teams$Playoff[teams$WCWin == 'Y'] <- 'Y'
# convert Playoff to factor 
teams$Playoff <- factor(teams$Playoff)


wildcard_era_teams <- teams[teams$yearID >= 1995, ]
table(wildcard_era_teams$G == 144, wildcard_era_teams$yearID)

# use 1996 as cutoff
with(wildcard_era_teams, table(yearID == 1995, G))


wildcard_era_teams <- teams[teams$yearID >= 1996, ]


ggplot(subset(wildcard_era_teams, Playoff == 'Y'), aes(W)) + 
  geom_histogram(fill = 'light blue', color = 'black', binwidth = 1) + 
  geom_vline(xintercept = mean(wildcard_era_teams$W[wildcard_era_teams$Playoff == 'Y']), color = 'red', linetype = 'longdash') +
  geom_vline(xintercept = median(wildcard_era_teams$W[wildcard_era_teams$Playoff == 'Y']), color = 'green', linetype = 'longdash') +
  xlab('Frequency') + ylab('Wins') +
  ggtitle('Histogram of Win Counts\nWith lines at mean and median')


ggplot(wildcard_era_teams, aes(W, color = Playoff)) + 
  geom_histogram(position = 'dodge', binwidth = 2) +
  coord_cartesian(xlim=c(50,120)) +
  geom_vline(xintercept = 94, linetype = 2) +
  xlab('Wins') + ylab('Count') +
  ggtitle('Histogram of Wins Sorted By Playoff Appearance\nWith line at mean of 94')


# only care about that lonely one team with the True Positive within the cunfusion matrix
table(wildcard_era_teams$W > 94, wildcard_era_teams$Playoff == 'N')

paste0("With an accuracy of ", round(407 /(407 + 1), 3))

# And looking at the data further
over94_no_playoff <- wildcard_era_teams[(wildcard_era_teams$W >= 94) & 
                                          (wildcard_era_teams$Playoff == 'N'), ]
paste0("Team with 96 wins and didn't make playoffs is The ", over94_no_playoff$name)
