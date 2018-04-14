library(tidyverse)
library(ggplot2)
library(caret)
library(readr)

Teams <- read_csv("Master/baseballdatabank-master/core/Teams.csv")
Salaries <- read_csv("Master/baseballdatabank-master/core/Salaries.csv")

Teams$Playoff <- Teams$DivWin

Teams$Playoff[Teams$WCWin == 'Y'] <- 'Y'

Teams$Playoff <- factor(Teams$Playoff)

Teams <- filter(Teams, yearID > 1969)
