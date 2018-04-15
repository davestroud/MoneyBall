library(tidyverse)
library(ggplot2)
library(caret)
library(readr)

# Libraries from Dr. Turners example
library(glmnet)
library(ROCR)
library(MASS)

library(ISLR)

Teams <- read_csv("/Users/davidstroud/Dropbox/Stats2/MoneyBall/Master/baseballdatabank-master/core/Teams.csv")
Salaries <- read_csv("/Users/davidstroud/Dropbox/Stats2/MoneyBall/Master/baseballdatabank-master/core/Salaries.csv")

# New year column, predictor, for teams that make the playoffs
Teams$Playoff <- Teams$DivWin

# Teams$Playoff[Teams$WCWin == 'Y'] <- 'Y'

# Turn Playoff into a factor variable
Teams$Playoff <- factor(Teams$Playoff)

# Teams after 1969 ~ Year that division wins were recognized.
Teams <- filter(Teams, yearID > 1969)

dim(Teams)
glimpse(Teams)

#lets attach newAuto so we don't have to keep writing Teams$
attach(Teams)

ftable(addmargins(table(teamID,Playoff)))

ggplot(Teams, aes(teamID, W, color = Playoff)) + geom_point() + 
  theme(axis.text.x = element_text(angle = 45)) +
  xlab('Team') + ylab('Wins') + 
  geom_hline(yintercept = 94, linetype = 2) +
  ggtitle('Wins By Team Colored By Playoffs Appearances')

team_wins <- Teams %>%
  group_by(teamID, yearID, lgID, divID, Playoff) %>%
  summarise(W = W) %>%
  ungroup() %>%
  arrange(teamID)

# Note that the National League Central started in 1994
nl <- ggplot(subset(team_wins, (lgID == 'NL')), aes(yearID, W)) + 
  geom_line(aes(color = teamID)) +
  facet_wrap(~ divID, nrow = 3) +
  xlab('Year') + ylab('Wins') +
  ylim(50,100) +
  ggtitle('Wins By Year - National League')

al <- ggplot(subset(team_wins, (lgID == 'AL')), aes(yearID, W)) + 
  geom_line(aes(color = teamID)) +
  facet_wrap(~ divID, nrow = 3) +
  ylim(50,100) +
  xlab('Year') + ylab('') +
  ggtitle('Wins By Year -  American League')

grid.arrange(nl, al, ncol = 2, name = 'Wins By Year Per League and Divison' )


ggplot(Teams, aes(teamID, W)) + 
  geom_boxplot(fill = 'blue') + 
  geom_hline(yintercept = 94, linetype = 2, color = 'black') +
  geom_hline(yintercept = 88, linetype = 2, color = 'red') + 
  theme(axis.text.x = element_text(angle = 90)) +
  xlab('Team Name') + ylab('Wins') +
  ggtitle('Boxplots of Teams vs Wins')


# transform NA for sac flies and hit by pitches to O so can compute correlation matrix 
Teams[is.na(Teams$SF)] <- 0
Teams$HBP[is.na(Teams$HBP)] <- 0

set.seed(101)
split <- sample.split(Teams$Playoff, 0.6)
train <- subset(Teams, split == TRUE)
test <- subset(Teams, split == FALSE)
dim(train)


model <- glm(Playoff ~ G+W+L+R+AB+H+HR+BB+SO+SB+CS+HBP+SF+RA+ER
             +ERA+CG+SHO+SV+IPouts, family = "binomial", train)


