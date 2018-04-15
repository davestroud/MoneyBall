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

# Turn Playoff into a factor variable
Teams$Playoff <- factor(Teams$Playoff)

# Teams after 1969 ~ Year that division wins were recognized.
Teams <- filter(Teams, yearID > 1969)

dim(Teams)
glimpse(Teams)

# attach so we don't have to keep writing Teams$
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
# National League plot
nl <- ggplot(subset(team_wins, (lgID == 'NL')), aes(yearID, W)) + 
  geom_line(aes(color = teamID)) +
  facet_wrap(~ divID, nrow = 3) +
  xlab('Year') + ylab('Wins') +
  ylim(50,100) +
  ggtitle('Wins By Year - National League')

# American League plot 
al <- ggplot(subset(team_wins, (lgID == 'AL')), aes(yearID, W)) + 
  geom_line(aes(color = teamID)) +
  facet_wrap(~ divID, nrow = 3) +
  ylim(50,100) +
  xlab('Year') + ylab('') +
  ggtitle('Wins By Year -  American League')

# Grid to review plots
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

# Set seed and split teams between training and testing data
# Note the 60/40 split for logistical regression
set.seed(101)
split <- sample.split(Teams$Playoff, 0.6)
train <- subset(Teams, split == TRUE)
test <- subset(Teams, split == FALSE)
dim(train)

# change Playoff to 1 for Y and 0 for N to work with ROCR
train$Playoff <- ifelse(train$Playoff == 'Y', 1, 0)


# Fit glm model: model
model <- glm(Playoff~G+W+L+R+AB+H+HR+BB+SO+SB+CS+HBP+RA+ER
             +ERA+CG+SHO+SV+IPouts, family = "binomial", train)

# Predict on test: p
p <- predict(model, test, type = "response")

# Calculate class probabilities: p_class
p_class <- ifelse(p > 0.50, 0,1 )


# https://stackoverflow.com/questions/19871043/r-package
# -caret-confusionmatrix-with-missing-categories
# overide to get confusion matrix to work
library(e1071)
u = union(p_class, test$Class)
t = table(factor(p_class, u), factor(test$Playoff, u))
confusionMatrix(t)







