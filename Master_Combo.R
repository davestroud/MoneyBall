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
library('psych')
library(caret)
library(readr)
library(glmnet)
library(bestglm)
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
library(e1071)



Teams <- read_csv("Master/baseballdatabank-master/core/Teams.csv")


# Create new column (predictor) for teams that make the playoffs; which includes both Division
# winners as well as wildcard winners (post 1995).

# this adds the the division winners to new predictor
Teams$Playoff <- Teams$DivWin

# this adds the wild card winners to new predictor
Teams$Playoff[Teams$WCWin == 'Y'] <- 'Y'

# Teams after 1969 ~ Year that division wins were recognized.
Teams <- Teams[Teams$yearID > 1969,]


Teams$teamID[Teams$teamID == 'CAL'] <- 'LAA'
Teams$teamID[Teams$teamID == 'FLO'] <- 'MIA'
Teams$teamID[Teams$teamID == 'ML4'] <- 'MIL'
Teams$teamID[Teams$teamID == 'MON'] <- 'WAS'
Teams$teamID[Teams$teamID == 'ANA'] <- 'LAA'
Teams$teamID[Teams$teamID == 'WS2'] <- 'TEX'
Teams$teamID[Teams$teamID == 'SE1'] <- 'MIL'

# Elimination of 1994 Season - No Postseason; 1981 - Shortened Season
Teams<-Teams[!(Teams$yearID=="1994"),]
Teams<-Teams[!(Teams$yearID=="1981"),]

# transform NA for sac flies and hit by pitches to O so can compute correlation matrix 
Teams$SF[is.na(Teams$SF)] <- 0
Teams$HBP[is.na(Teams$HBP)] <- 0

# convert Playoff to factor 
Teams$Playoff <- factor(Teams$Playoff)
#summary(Teams$Playoff)
#str(Teams$Playoff)

#############################################
### Exploratory Data Analysis
#############################################
#  dim(Teams)
# summary(Teams)
# glimpse(Teams)

# Creating index for numerical variable in data set
index.Teams.numeric<-which(sapply(Teams,is.numeric))

attach(Teams)
#Visualize
par(mfrow=c(3,3))
plot(Playoff~HR,col=c("red","blue"))
plot(Playoff~RA,col=c("red","blue"))
plot(Playoff~SV,col=c("red","blue"))
plot(Playoff~BBA,col=c("red","blue"))
plot(Playoff~SOA,col=c("red","blue"))
plot(Playoff~E,col=c("red","blue"))
plot(Playoff~AB,col=c("red","blue"))
plot(Playoff~R,col=c("red","blue"))

par(mfrow=c(1,1))


# Examine the correlation between the continous predictors
# Create a correlation matrix for continuous prdictors
pairs(Teams[,c("HR","RA","SV","BBA","SOA","E","AB","R")])


# With many predictors, heatmap with correlations are helpful
# to examine redundancy. No scale because correlations are already scaled
# Variables used"AB", "H", "X2B", "X3B", "HR", "BB", "SO", "SB", "RA", "ER", "ERA", "CG", "SHO", "SV", "IPouts", "HA", "HRA", "BBA", "SOA", "E", "FP", "BPF", "PPF"
my.cor<-cor(Teams[,index.Teams.numeric])
heatmap.2(my.cor,col=redblue(75), 
          density.info="none", trace="none", dendrogram=c("row"), 
          symm=F,symkey=T,symbreaks=T, scale="none")

#Note above. You can also use the plots previously to examine one by one
#if predictors are associated category to category or category to continuous.

#Create a PCA of the continous predictors, to visualize if they seperate out.
pc.results <-prcomp(Teams[,index.Teams.numeric],scale. = TRUE)
pc.scores<-pc.results$x
pc.scores<-data.frame(pc.scores)
pc.scores$Playoff<-Teams$Playoff

ggplot(dat = pc.scores, aes(x=PC1, y = PC2))+
  geom_point(aes(col=Playoff), size=1)+
  ggtitle("PCA of MLB Playoff Participants")+
  theme(plot.title = element_text(hjust = 0.5))


# Creation of Wins by Team Graphics          
ftable(addmargins(table(teamID,Playoff)))

ggplot(Teams, aes(teamID, W, color = Playoff)) + geom_point() + 
  theme(axis.text.x = element_text(angle = 90)) +
  xlab('Team') + ylab('Wins') + 
  geom_hline(yintercept = 94, linetype = 2) +
  ggtitle('Wins By Team Colored By Playoffs Appearances')+
  theme(plot.title = element_text(hjust = 0.5))

team_wins <- Teams %>%
  group_by(teamID, yearID, lgID, divID, Playoff) %>%
  summarise(W = W) %>%
  ungroup() %>%
  arrange(teamID)



ggplot(subset(Teams, Playoff == 'Y'), aes(W)) + 
  geom_histogram(fill = 'light green', color = 'black', binwidth = 1) + 
  geom_vline(xintercept = mean(Teams$W[Teams$Playoff == 'Y']), color = 'red', linetype = 'longdash') +
  geom_vline(xintercept = median(Teams$W[Teams$Playoff == 'Y']), color = 'green', linetype = 'longdash') +
  xlab('Frequency') + ylab('Wins') +
  ggtitle('Histogram of Win Counts\nWith lines at mean and median') +
  theme(plot.title = element_text(hjust = 0.5))






# Note that the National League Central started in 1994
# National League plot
nl <- ggplot(subset(team_wins, (lgID == 'NL')), aes(yearID, W)) + 
  geom_line(aes(color = teamID)) +
  facet_wrap(~ divID, nrow = 3) +
  xlab('Year') + ylab('Wins') +
  ylim(50,100) +
  ggtitle('Wins By Year - National League') +
  theme(plot.title = element_text(hjust = 0.5))

# American League plot 
al <- ggplot(subset(team_wins, (lgID == 'AL')), aes(yearID, W)) + 
  geom_line(aes(color = teamID)) +
  facet_wrap(~ divID, nrow = 3) +
  ylim(50,100) +
  xlab('Year') + ylab('') +
  ggtitle('Wins By Year -  American League') +
  theme(plot.title = element_text(hjust = 0.5))

# Grid to review plots
grid.arrange(nl, al, ncol = 2, name = 'Wins By Year Per League and Divison' )

ggplot(Teams, aes(teamID, W)) + 
  geom_boxplot(fill = 'green') + 
  geom_hline(yintercept = 94, linetype = 2, color = 'black') +
  theme(axis.text.x = element_text(angle = 90)) +
  xlab('Team Name') + ylab('Wins') +
  ggtitle('Boxplots of Teams vs Wins') +
  theme(plot.title = element_text(hjust = 0.5))





#############################################
### Analysis - LASSO Data Analysis
#############################################          

# Convert Playoff to 1 = 'Y' and 0 = 'N'
# Teams$Playoff <- ifelse(Teams$Playoff == 'Y', 1, 0)



# Set seed and split teams between training and testing data
# Note the 90/10 split for logistical regression
set.seed(1606)
split <- sample.split(Teams$Playoff, 0.9)
team.train <- subset(Teams, split == TRUE)
team.test <- subset(Teams, split == FALSE)        
# dim(team.train)
# glimpse(team.train)
# dim(team.test)
# glimpse(team.test)

# Selecting Relavant variables for analysis:  "AB", "H", "X2B", "X3B", "HR",
# "BB", "SO", "SB", "RA", "ER", "ERA", "CG", "SHO", "SV", "IPouts",
#  "HA", "HRA", "BBA", "SOA", "E", "FP", "BPF", "PPF"

team.train.relevant <- team.train
team.train.relevant <- team.train.relevant  %>% na.omit()

#######################################
##### Training Dataset Analysis #######

#Select only numeric variables relevant to our investigation
team.train.x <- team.train.relevant[,c("AB","H","X2B","X3B","HR","BB","SO","SB","RA",
                                       "ER","ERA","CG","SHO","SV","IPouts","HA","HRA",
                                       "BBA","SOA","E","FP","BPF","PPF")]

team.train.y <- team.train.relevant$Playoff
team.train.y <- as.factor(as.character(team.train.y))
#  table(team.train.y)          

# GLMNet Requrires a matrix 
team.train.x <- as.matrix(team.train.x)

###### GLMNet - LASSO ####
cvfit <- cv.glmnet(team.train.x, team.train.y, family = "binomial", type.measure = "class", nlambda = 1000)

# Plotting cvfit /Misclassification Error - Training Dataset
plot(cvfit)

# Displaying LASSO variable coefficients - for Min lambda 
coef(cvfit, s = cvfit$lambda.min)

# Displaying Optimal penalty lambda.Min for LASSO  
lambmin <- cvfit$lambda.min
cat("Optimal Lambda Value ", lambmin,"\n")

# Summary statistics for LASSO analysis
#  summary(cvfit)

# CV misclassification error rate:
cvfit$cvm[which(cvfit$lambda==cvfit$lambda.min)]

# Predicted Values - Training Dataset
fit.pred <- predict(cvfit, newx = team.train.x, type = "response")

# Create ROC curves - Training Dataset. Results are biased but lets create ROC's.
# These are predicted probabilities from logistic model  exp(b)/(1+exp(b))
pred <- prediction(fit.pred[,1], team.train.y)
roc.perf = performance(pred, measure = "tpr", x.measure = "fpr")
auc.train <- performance(pred, measure = "auc")
auc.train <- auc.train@y.values

# Plot ROC - Training Dataset
plot(roc.perf)
abline(a=0, b= 1) #Ref line indicating poor performance
text(x = .40, y = .6,paste("AUC = ", round(auc.train[[1]],3), sep = ""))

# Histogram of Predicted Values
hist(fit.pred)

# Printing Training Dataset Predicted Valves
# print(fit.pred)

# If you want to look at other error rates in terms of prediction perfromance
# Note the predictor class is the last level of the response in alphabetical
# order. So for this data set, 'Y' is the predictor class where predicted
# probabilities greater than .5 are assigned to 'Y'.
lasso.probs<-fit.pred
lasso.pred<-rep("N",length(fit.pred))
lasso.prob.cutoff <- 0.5
lasso.pred[lasso.probs>lasso.prob.cutoff]="Y"
Truth<-team.train.y
Pred<-lasso.pred
ftable(addmargins(table(Pred,Truth)))

# Misclassiciation rate
x<-table(Pred,Truth)
ME<-(x[1,2]+x[2,1])/sum(x)

# False positive rate
FP<-x[2,1]/(x[2,1]+x[1,1])

# False negative rate
FN<-x[1,2]/(x[1,2]+x[2,2])

# Displaying Classification Rates
cat("LASSO Probability Cutoff ", lasso.prob.cutoff,"\n")
cat("Misclassification Rate ", ME, "%","\n")
cat("False Positive Rate ", FP, "%","\n")
cat("False Negative Rate ", FN, "%","\n")  

#summary(team.train$Playoff)
#print(table(Pred))






dat.train.x <- model.matrix(Playoff~HR+RA+SV+BBA+SOA+E+AB+R-1,Teams)
dat.train.y<-Teams[,"Playoff"]

cvfit <- cv.glmnet(dat.train.x, dat.train.y, family = "binomial", type.measure = "class", nlambda = 1000)
par(mfrow=c(1,1))
plot(cvfit)
coef(cvfit, s = "lambda.min")
#CV misclassification error rate is little below .1
cvfit$cvm[which(cvfit$lambda==cvfit$lambda.min)]

#Optimal penalty
cvfit$lambda.min


#For final model predictions go ahead and refit lasso using entire
#data set
finalmodel<-glmnet(dat.train.x, dat.train.y, family = "binomial",lambda=cvfit$lambda.min)

#Get training set predictions...We know they are biased but lets create ROC's.
#These are predicted probabilities from logistic model  exp(b)/(1+exp(b))
fit.pred <- predict(finalmodel, newx = dat.train.x, type = "response")

#Create ROC curves (Remember if you have a test data set, you can use that to compare models)
library(ROCR)
pred <- prediction(fit.pred[,1], dat.train.y)
roc.perf = performance(pred, measure = "tpr", x.measure = "fpr")
auc.train <- performance(pred, measure = "auc")
auc.train <- auc.train@y.values

#Plot ROC
plot(roc.perf,main="LASSO")
abline(a=0, b= 1) #Ref line indicating poor performance
text(x = .40, y = .6,paste("AUC = ", round(auc.train[[1]],3), sep = ""))


#In addition to LASSO, if we are concerned that the biased estiamtes
#are affecting our model, we can go back and refit using regular 
#regression removing the variables that have no importance.
coef(finalmodel)

olog<-glm(Playoff~HR+RA+SV+BBA+SOA+E+AB+R,data=Teams,family=binomial)
fit.pred <- predict(olog, newx = dat.train.x, type = "response")

pred <- prediction(fit.pred, dat.train.y)
roc.perf = performance(pred, measure = "tpr", x.measure = "fpr")
auc.train <- performance(pred, measure = "auc")
auc.train <- auc.train@y.values

#Plot ROC
plot(roc.perf,main="Ordingary Logistic")
abline(a=0, b= 1) #Ref line indicating poor performance
text(x = .40, y = .6,paste("AUC = ", round(auc.train[[1]],3), sep = ""))

#print(fit.pred)
#If you want to look at other error rates in terms of prediction perfromance
#Note the predictor class is the last level of the response in alphabetical
#order. So for this data set, LOW is the predictor class where predicted
#probabilities greater than .5 are assigned to LOW.
lasso.probs<-fit.pred
lasso.pred<-rep("N",length(fit.pred))
lasso.pred[lasso.probs>.45]="Y"
Truth<-dat.train.y
Pred<-lasso.pred
ftable(addmargins(table(Pred,Truth)))

#Brian add
coef(cvfit, s=cvfit$lambda.min)
summary(cvfit)

#Misclassiciation rate
x<-table(Pred,Truth)
ME<-(x[1,2]+x[2,1])/sum(x)
#False positive rate
FP<-x[2,1]/(x[2,1]+x[1,1])
#False negative rate
FN<-x[1,2]/(x[1,2]+x[2,2])
ME
FP
FN

#summary(Teams$Playoff)
#print(table(Pred))



#In this case they are not much different but they can be.
#glm or proc logistic will be the go to spot if you want inference
#and hypothesis testing

summary(olog)
plot(olog) #Residual plots but again, they are not very helpful.


#############################
#############################
#################

# Convert Playoff to 1 = 'Y' and 0 = 'N'
Teams$Playoff <- ifelse(Teams$Playoff == 'Y', 1, 0)



# Set seed and split teams between training and testing data
# Note the 90/10 split for logistical regression
set.seed(1606)
split <- sample.split(Teams$Playoff, 0.9)
team.train <- subset(Teams, split == TRUE)
team.test <- subset(Teams, split == FALSE)

# dim(team.train)
# glimpse(team.train)

team.train.relevant <- team.train  %>%
  dplyr::select("teamID", "G", "R", "AB", "H", "X2B", "X3B", "HR", "BB", "SO", "SB", "CS", "RA", "ER", "ERA", "CG", "SHO", "SV", "IPouts", "HA", "HRA", "BBA", "SOA", "E", "DP", "FP", "attendance", "BPF", "PPF", "Playoff")

team.train.relevant <- team.train.relevant  %>% na.omit()

#select only numeric rows relevant to our investigation
team.train.x <- team.train.relevant  %>%
  dplyr::select("R", "AB", "H", "X2B", "X3B", "HR", "BB", "SO", "SB", "CS", "RA", "ER", "ERA", "CG", "SHO", "SV", "IPouts", "HA", "HRA", "BBA", "SOA", "E", "DP", "FP","BPF","PPF")
#  dplyr::select("G", "R", "AB", "H", "X2B", "X3B", "HR", "BB", "SO", "SB", "CS", "RA", "ER", "ERA", "CG", "SHO", "SV", "IPouts", "HA", "HRA", "BBA", "SOA", "E", "DP", "FP", "attendance", "BPF", "PPF")

team.train.y <- team.train.relevant$Playoff
team.train.y <- as.factor(as.character(team.train.y))
table(team.train.y)

#glmnet requires a matrix 
team.train.x <- as.matrix(team.train.x)

cvfit <- cv.glmnet(team.train.x, team.train.y, family = "binomial", type.measure = "class", nlambda = 1000)
plot(cvfit)
coef(cvfit, s = "lambda.min")

#CV misclassification error rate:
cvfit$cvm[which(cvfit$lambda==cvfit$lambda.min)]

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
par(mflow=c(1,1))
hist(fit.pred)
#print(fit.pred)

#If you want to look at other error rates in terms of prediction perfromance
#Note the predictor class is the last level of the response in alphabetical
#order. So for this data set, LOW is the predictor class where predicted
#probabilities greater than .5 are assigned to LOW.
lasso.probs<-fit.pred
lasso.pred<-rep("N",length(fit.pred))
lasso.pred[lasso.probs>.5]="Y"
Truth<-team.train.y
Pred<-lasso.pred
ftable(addmargins(table(Pred,Truth)))

#Misclassiciation rate
x<-table(Pred,Truth)
ME<-(x[1,2]+x[2,1])/sum(x)
#False positive rate
FP<-x[2,1]/(x[2,1]+x[1,1])
#False negative rate
FN<-x[1,2]/(x[1,2]+x[2,2])
ME
FP
FN

#summary(team.train$Playoff)
#print(table(Pred))






#############
#############
#Get Validation Set I
team.test.relevant <- team.test %>%
  dplyr::select("teamID", "G", "R", "AB", "H", "X2B", "X3B", "HR", "BB", "SO", "SB", "CS", "RA", "ER", "ERA", "CG", "SHO", "SV", "IPouts", "HA", "HRA", "BBA", "SOA", "E", "DP", "FP", "attendance", "BPF", "PPF", "Playoff")

team.test.relevant <- team.test.relevant  %>% na.omit()

#select only numeric rows relevant to our investigation
team.test.x <- team.test.relevant  %>%
  dplyr::select("R", "AB", "H", "X2B", "X3B", "HR", "BB", "SO", "SB", "CS", "RA", "ER", "ERA", "CG", "SHO", "SV", "IPouts", "HA", "HRA", "BBA", "SOA", "E", "DP", "FP","BPF","PPF")
#    dplyr::select("G", "R", "AB", "H", "X2B", "X3B", "HR", "BB", "SO", "SB", "CS", "RA", "ER", "ERA", "CG", "SHO", "SV", "IPouts", "HA", "HRA", "BBA", "SOA", "E", "DP", "FP", "attendance", "BPF", "PPF")

team.test.y <- team.test.relevant$Playoff
team.test.y <- as.factor(as.character(team.test.y))
#table(team.test.y)

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


#hist(fit.pred1)
#print(fit.pred1)

#If you want to look at other error rates in terms of prediction perfromance
#Note the predictor class is the last level of the response in alphabetical
#order. So for this data set, LOW is the predictor class where predicted
#probabilities greater than .5 are assigned to LOW.
lasso.probs1<-fit.pred1
lasso.pred1<-rep("N",length(fit.pred1))
lasso.pred1[lasso.probs1>.3]="Y"
Truth1<-team.test.y
Pred1<-lasso.pred1
ftable(addmargins(table(Pred1,Truth1)))

#Misclassiciation rate
x1<-table(Pred1,Truth1)
ME1<-(x1[1,2]+x1[2,1])/sum(x1)
#False positive rate
FP1<-x1[2,1]/(x1[2,1]+x1[1,1])
#False negative rate
FN1<-x1[1,2]/(x1[1,2]+x1[2,2])
ME1
FP1
FN1

#summary(team.test$Playoff)
#print(table(Pred1))





########################################################

#LDA
#select only numeric rows relevant to our investigation
team.train.x <- team.train.relevant  %>%
  dplyr::select("R", "AB", "H", "X2B", "X3B", "HR", "BB", "SO", "SB", "CS", "RA", "ER", "ERA", "CG", "SHO", "SV", "IPouts", "HA", "HRA", "BBA", "SOA", "E", "DP", "FP","BPF","PPF")
#    dplyr::select("G", "R", "AB", "H", "X2B", "X3B", "HR", "BB", "SO", "SB", "CS", "RA", "ER", "ERA", "CG", "SHO", "SV", "IPouts", "HA", "HRA", "BBA", "SOA", "E", "DP", "FP", "attendance", "BPF", "PPF")

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
  dplyr::select("R", "AB", "H", "X2B", "X3B", "HR", "BB", "SO", "SB", "CS", "RA", "ER", "ERA", "CG", "SHO", "SV", "IPouts", "HA", "HRA", "BBA", "SOA", "E", "DP", "FP","BPF","PPF")
#    dplyr::select("G", "R", "AB", "H", "X2B", "X3B", "HR", "BB", "SO", "SB", "CS", "RA", "ER", "ERA", "CG", "SHO", "SV", "IPouts", "HA", "HRA", "BBA", "SOA", "E", "DP", "FP", "attendance", "BPF", "PPF")
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
# caret-confusionmatrix-with-missing-categories
# overide to get confusion matrix to work
u = union(p_class, team.test$Playoff)
t = table(factor(p_class, u), factor(team.test$Playoff, u))
confusionMatrix(t)



#Brian add
#########################################
#########################################


set.seed(1606)
split <- sample.split(Teams$Playoff, 0.9)
team.train <- subset(Teams, split == TRUE)
team.test <- subset(Teams, split == FALSE)

#getting rid of some fields with many NA fields and trimming to only relevant numerics
team.train <- team.train  %>%
  dplyr::select("Playoff", "AB", "H", "X2B", "X3B", "HR", "BB", "SO", "SB", "RA", "ER", "ERA", "CG", "SHO", "SV", "IPouts", "HA", "HRA", "BBA", "SOA", "E", "FP", "BPF", "PPF")

#getting rid of some fields with many NA fields and trimming to only relevant numerics
team.test <- team.test  %>%
  dplyr::select("Playoff", "AB", "H", "X2B", "X3B", "HR", "BB", "SO", "SB", "RA", "ER", "ERA", "CG", "SHO", "SV", "IPouts", "HA", "HRA", "BBA", "SOA", "E", "FP", "BPF", "PPF")


#Omitting all remaining NA Values, shrinking from 2256 records to 2095 records
team.train <- team.train %>% na.omit()

#Omitting all remaining NA Values, kept for consistency and sustainability even though there were none in our initial test
team.test <- team.test %>% na.omit()

index.train<-which(sapply(team.train,is.numeric))
index.test<-which(sapply(team.test,is.numeric))

#How are the predictors correlated to each other. Anything aggregious we may want to go ahead
#and make a call.
cor.mat<-cor(team.train[,index.train])
heatmap.2(cor.mat,,density.info="none", trace="none",col=redgreen(75),scale="none")

set.seed(1) 
full.model<-lm(Playoff~.,team.train)
par(mfrow=c(1,1))
plot(full.model)
summary(full.model)

testMSE_full<-mean((team.test$Playoff-predict(full.model,team.test))^2)
testMSE_full

#Forward Selection
null=lm(Playoff~1, data=team.train) 
null 

full=lm(Playoff~., data=team.train)
full 

step(null, scope=list(lower=null, upper=full), direction="forward") 

#LASSO
#LASSO like Ridge t
x=model.matrix(Playoff~.,team.train)[,-1]
y=team.train$Playoff

xtest<-model.matrix(Playoff~.,team.test)[,-1]
ytest<-team.test$Playoff

# Data = considering that we have a data frame named dataF, with its first column being the class
x=model.matrix(Playoff~.,team.train)[,-1]
y <- team.train$Playoff

# Fitting the model (Lasso: Alpha = 1)
set.seed(999)
lasso.mod <- cv.glmnet(x, y, family='binomial', alpha=1, parallel=TRUE, standardize=TRUE, type.measure='auc')

# Results
plot(lasso.mod)
plot(lasso.mod$glmnet.fit, xvar="lambda", label=TRUE)
lasso.mod$lambda.min
lasso.mod$lambda.1se
#output the coefficients of lasso
coef(lasso.mod, s=lasso.mod$lambda.min)
summary(lasso.mod)


cv.out=cv.glmnet(x,y,alpha=1, family="binomial") #alpha=1 performs LASSO
plot(cv.out)
bestlambda<-cv.out$lambda.min
lasso.pred=predict (lasso.mod ,s=bestlambda ,newx=xtest)

testMSE_LASSO<-mean((ytest-lasso.pred)^2)
testMSE_LASSO
