library(tidyverse)
library(ggplot2)
library(caret)
library(readr)

library(glmnet)
library(ROCR)
library(MASS)


Teams <- read_csv("/Users/davidstroud/Dropbox/Stats2/MoneyBall/Master/baseballdatabank-master/core/Teams.csv")
Salaries <- read_csv("/Users/davidstroud/Dropbox/Stats2/MoneyBall/Master/baseballdatabank-master/core/Salaries.csv")

# New year column, predictor, for teams that make the playoffs
Teams$Playoff <- Teams$DivWin

# Teams$Playoff[Teams$WCWin == 'Y'] <- 'Y'

# Turn Playoff into a factor variable
Teams$Playoff <- factor(Teams$Playoff)

# Teams after 1969 ~ Year that division wins were recognized.
Teams <- filter(Teams, yearID > 1969)
