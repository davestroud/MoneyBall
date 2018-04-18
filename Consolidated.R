#########################################################
## MSDS 6372 - Project 2
## Brian Coari Jeffrey Lancon & David Stroud
## 2018/04/20
##  
## What we plan to do and what we are trying to accomplish.
## ??
## ??
## ??
## ??
## ??
#########################################################
# Find all packages that are not in the list of installed packages and install packages used if not yet installed
list.of.packages <- c('ggplot2','tidyverse','gridExtra','caTools','coorplot','memisc','rpart','rpart.plot','gbm','caret','readr','xml2','rvest', 'dplyr', 'tidyr', 'xtable','readr','glmnet','ISLR','leaps','gplots','ROCR','MASS')
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages,repos="http://cran.rstudio.com/")

# Initializing library packages
library(ggplot2)
library(tidyverse)
library(gridExtra)
library(caTools)
library(corrplot)
library(memisc)
library(rpart)
library(rpart.plot)
library(gbm)
library(caret)
library(readr)
# Libraries from Dr. Turners example
library(glmnet)
library(ROCR)
library(MASS)
library(ISLR)
library(e1071) # Used for confusion matrix
library(dplyr)
library('xml2')
library('rvest')
library('tidyr')
library('xtable')
library('readr')
library(leaps)
library(boot)
library(gplots)

# create a temp file
temp <- tempfile()
# Baseball Zip File Url
url <- "http://seanlahman.com/files/database/baseballdatabank-master_2018-03-28.zip"
# fetch the file into the temp. file and download it
download.file(url, temp, mode="wb")
# extract the target file from temp. file
table1 <- unz(temp, "baseballdatabank-master/core/Teams.csv")
Teams <- read.csv(table1, sep=",", header=T)


# Create new column (predictor) for teams that make the playoffs; which includes both Division
# winners as well as wildcard winners (post 1995).

# this adds the the division winners to new predictor
Teams$Playoff <- Teams$DivWin
      
# this adds the wild card winners to new predictor
Teams$Playoff[Teams$WCWin == 'Y'] <- 'Y'

# convert Playoff to factor 
Teams$Playoff <- factor(Teams$Playoff)

# Teams after 1969 ~ Year that division wins were recognized.
Teams <- Teams[Teams$yearID > 1969,]


Teams$teamID[Teams$teamID == 'CAL'] <- 'LAA'
Teams$teamID[Teams$teamID == 'FLO'] <- 'MIA'
Teams$teamID[Teams$teamID == 'ML4'] <- 'MIL'
Teams$teamID[Teams$teamID == 'MON'] <- 'WAS'
Teams$teamID[Teams$teamID == 'ANA'] <- 'LAA'
Teams$teamID[Teams$teamID == 'WS2'] <- 'TEX'
Teams$teamID[Teams$teamID == 'SE1'] <- 'MIL'

# Elimination of 1994 Season - No Postseason
Teams<-Teams[!(Teams$yearID=="1994"),]

# transform NA for sac flies and hit by pitches to O so can compute correlation matrix 
Teams$SF[is.na(Teams$SF)] <- 0
Teams$HBP[is.na(Teams$HBP)] <- 0

# SHOULD ADD SOME ADDITIONAL PRELIMINARY DATA ANALYSIS
#####

dim(Teams)
summary(Teams)
glimpse(Teams)

# attach so we don't have to keep writing Teams$
attach(Teams)

ftable(addmargins(table(teamID,Playoff)))

ggplot(subset(Teams, Playoff == 'Y'), aes(W)) + 
  geom_histogram(fill = 'light blue', color = 'black', binwidth = 1) + 
  geom_vline(xintercept = mean(Teams$W[Teams$Playoff == 'Y']), color = 'red', linetype = 'longdash') +
  geom_vline(xintercept = median(Teams$W[Teams$Playoff == 'Y']), color = 'green', linetype = 'longdash') +
  xlab('Frequency') + ylab('Wins') +
  ggtitle('Histogram of Win Counts\nWith lines at mean and median')




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

###########

# Convert Playoff to 1 = 'Y' and 0 = 'N'
Teams$Playoff <- ifelse(Teams$Playoff == 'Y', 1, 0)


# Set seed and split teams between training and testing data
# Note the 90/10 split for logistical regression
set.seed(1606)
split <- sample.split(Teams$Playoff, 0.9)
team.train <- subset(Teams, split == TRUE)
team.test <- subset(Teams, split == FALSE)
dim(team.train)


team.train.relevant <- team.train  %>%
  dplyr::select("teamID", "G", "R", "AB", "H", "X2B", "X3B", "HR", "BB", "SO", "SB", "CS", "RA", "ER", "ERA", "CG", "SHO", "SV", "IPouts", "HA", "HRA", "BBA", "SOA", "E", "DP", "FP", "attendance", "BPF", "PPF", "Playoff")

team.train.relevant <- team.train.relevant  %>% na.omit()

#select only numeric rows relevant to our investigation
team.train.x <- team.train.relevant  %>%
  dplyr::select("G", "R", "AB", "H", "X2B", "X3B", "HR", "BB", "SO", "SB", "CS", "RA", "ER", "ERA", "CG", "SHO", "SV", "IPouts", "HA", "HRA", "BBA", "SOA", "E", "DP", "FP", "attendance", "BPF", "PPF")

team.train.y <- team.train.relevant$Playoff
team.train.y <- as.factor(as.character(team.train.y))

#glmnet requires a matrix 
team.train.x <- as.matrix(team.train.x)

cvfit <- cv.glmnet(team.train.x, team.train.y, family = "binomial", type.measure = "class", nlambda = 1000)
plot(cvfit)
coef(cvfit, s = "lambda.min")

#Get training set predictions...We know they are biased but lets create ROC's.
#These are predicted probabilities from logistic model  exp(b)/(1+exp(b))
fit.pred <- predict(cvfit, newx = team.train.x, type = "response")

#Create ROC curves
pred <- prediction(fit.pred[,1], team.train.y)
roc.perf = performance(pred, measure = "tpr", x.measure = "fpr")
auc.train <- performance(pred, measure = "auc")
auc.train <- auc.train@y.values

#Plot ROC
plot(roc.perf)
abline(a=0, b= 1) #Ref line indicating poor performance
text(x = .40, y = .6,paste("AUC = ", round(auc.train[[1]],3), sep = ""))


#############
#Get Validation Set I
team.test.relevant <- team.test %>%
  dplyr::select("teamID", "G", "R", "AB", "H", "X2B", "X3B", "HR", "BB", "SO", "SB", "CS", "RA", "ER", "ERA", "CG", "SHO", "SV", "IPouts", "HA", "HRA", "BBA", "SOA", "E", "DP", "FP", "attendance", "BPF", "PPF", "Playoff")

team.test.relevant <- team.test.relevant  %>% na.omit()

#select only numeric rows relevant to our investigation
team.test.x <- team.test.relevant  %>%
  dplyr::select("G", "R", "AB", "H", "X2B", "X3B", "HR", "BB", "SO", "SB", "CS", "RA", "ER", "ERA", "CG", "SHO", "SV", "IPouts", "HA", "HRA", "BBA", "SOA", "E", "DP", "FP", "attendance", "BPF", "PPF")
team.test.y <- team.test.relevant$Playoff
team.test.y <- as.factor(as.character(team.test.y))

#glmnet requires a matrix 
team.test.x <- as.matrix(team.test.x)

#Run model from training set on valid set I
fit.pred1 <- predict(cvfit, newx = team.test.x, type = "response")

#ROC curves
pred1 <- prediction(fit.pred1[,1], team.test.y)
roc.perf1 = performance(pred1, measure = "tpr", x.measure = "fpr")
auc.val1 <- performance(pred1, measure = "auc")
auc.val1 <- auc.val1@y.values
plot(roc.perf1)
abline(a=0, b= 1)
text(x = .40, y = .6,paste("AUC = ", round(auc.val1[[1]],3), sep = ""))

########################################################

#LDA
#select only numeric rows relevant to our investigation
team.train.x <- team.train.relevant  %>%
  dplyr::select("G", "R", "AB", "H", "X2B", "X3B", "HR", "BB", "SO", "SB", "CS", "RA", "ER", "ERA", "CG", "SHO", "SV", "IPouts", "HA", "HRA", "BBA", "SOA", "E", "DP", "FP", "attendance", "BPF", "PPF")

team.train.y <- team.train.relevant$Playoff
team.train.y <- as.factor(as.character(team.train.y))

fit.lda <- lda(team.train.y ~ ., data = team.train.x)
pred.lda <- predict(fit.lda, newdata = team.train.x)


preds <- pred.lda$posterior
preds <- as.data.frame(preds)

pred <- prediction(preds[,2],team.train.y)
roc.perf = performance(pred, measure = "tpr", x.measure = "fpr")
auc.train <- performance(pred, measure = "auc")
auc.train <- auc.train@y.values
plot(roc.perf)
abline(a=0, b= 1)
text(x = .40, y = .6,paste("AUC = ", round(auc.train[[1]],3), sep = ""))

#Valid set I
team.test.x <- team.test.relevant  %>%
  dplyr::select("G", "R", "AB", "H", "X2B", "X3B", "HR", "BB", "SO", "SB", "CS", "RA", "ER", "ERA", "CG", "SHO", "SV", "IPouts", "HA", "HRA", "BBA", "SOA", "E", "DP", "FP", "attendance", "BPF", "PPF")
team.test.y <- team.test.relevant$Playoff
team.test.y <- as.factor(as.character(team.test.y))


pred.lda1 <- predict(fit.lda, newdata = team.test.x)

preds1 <- pred.lda1$posterior
preds1 <- as.data.frame(preds1)

pred1 <- prediction(preds1[,2],team.test.y)
roc.perf = performance(pred1, measure = "tpr", x.measure = "fpr")
auc.train <- performance(pred1, measure = "auc")
auc.train <- auc.train@y.values
plot(roc.perf)
abline(a=0, b= 1)
text(x = .40, y = .6,paste("AUC = ", round(auc.train[[1]],3), sep = ""))






#########################################
#########################################



# Fit glm model: model
model <- glm(Playoff~G+Ghome+W+L+R+AB+H+HR+RA+ER
             +ERA+CG+SHO+SV+IPouts+HA+BBA+SOA+E
             ,family =binomial(link="logit"), team.train)

# Predict on test: p
p <- predict(model, team.test, type = "response")

# Calculate class probabilities: p_class
p_class <- ifelse(p > 0.50, 0,1 )

# https://stackoverflow.com/questions/19871043/r-package
# -caret-confusionmatrix-with-missing-categories
# overide to get confusion matrix to work
library(e1071)
u = union(p_class, team.test$Playoff)
t = table(factor(p_class, u), factor(team.test$Playoff, u))
confusionMatrix(t)
