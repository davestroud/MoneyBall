



# Find all packages that are not in the list of installed packages and install packages used if not yet installed
list.of.packages <- c('xml2','rvest', 'dplyr', 'tidyr', 'xtable','readr','glmnet','ISLR','leaps','gplots','ROCR','MASS')
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages,repos="http://cran.rstudio.com/")

#Load Libraries
library('xml2')
library('rvest')
library('dplyr')
library('tidyr')
library('xtable')
library('readr')
library(glmnet)
library(ISLR)
library(leaps)
library(boot)
library(gplots)
library(ROCR)
library(MASS)


# Baseball Zip File Url
url <- 'http://seanlahman.com/files/database/lahman-csv_2015-01-24.zip'
# create a temp file
temp <- tempfile()
# fetch the file into the temp. file and download it
download.file(url, temp)
# extract the target file from temp. file
team <- read.csv(unz(temp, 'Teams.csv'))
postseason <- read.csv(unz(temp, 'SeriesPost.csv'))


team$win_pct <- round(team$W/(team$W + team$L),4)
team$avg <- round(team$H/team$AB,4)

postseason_teams_ws_winners <- postseason %>% 
  filter(round == "WS") %>% 
  dplyr::select(yearID,team_id=teamIDwinner) %>% 
  dplyr::mutate(ps_team="1", ws_winner="1") %>% 
  unique()

postseason_teams_ps_losers <- postseason %>% 
  dplyr::select(yearID,team_id=teamIDloser) %>% 
  dplyr::mutate(ps_team="1", ws_winner="0") %>% 
  unique()

postseason_ps_teams <- rbind(postseason_teams_ws_winners,postseason_teams_ps_losers)


postseason_ps_teams<- rename(postseason_ps_teams, teamID = team_id)


team_merged = merge(team,postseason_ps_teams,by.x = c("yearID","teamID"), by.y = c("yearID","teamID"),all.x=TRUE,sort=FALSE)

#Set the ps_team and ws_winner variables to "0" for teams that were not in the postseason for their years
vars.to.replace <- c("ps_team", "ws_winner")
team_merged2 <- team_merged[vars.to.replace]
team_merged2[is.na(team_merged2)] <- "0"
team_merged[vars.to.replace] <- team_merged2

team.train <- team_merged[which(team_merged$year < 2008),]
team.test <- team_merged[which(team_merged$year >= 2008),]

team.train.relevant <- team.train  %>%
  dplyr::select("ps_team", "G", "win_pct", "R", "AB", "H", "X2B", "X3B", "HR", "BB", "SO", "SB", "CS", "RA", "ER", "ERA", "CG", "SHO", "SV", "IPouts", "HA", "HRA", "BBA", "SOA", "E", "DP", "FP", "attendance", "BPF", "PPF", "avg")

team.train.relevant <- team.train.relevant  %>% na.omit()

#select only numeric rows relevant to our investigation
team.train.x <- team.train.relevant  %>%
  dplyr::select("G", "win_pct", "R", "AB", "H", "X2B", "X3B", "HR", "BB", "SO", "SB", "CS", "RA", "ER", "ERA", "CG", "SHO", "SV", "IPouts", "HA", "HRA", "BBA", "SOA", "E", "DP", "FP", "attendance", "BPF", "PPF", "avg")

team.train.y <- team.train.relevant$ps_team
team.train.y <- as.factor(as.character(team.train.y))

#glmnet requires a matrix 
team.train.x <- as.matrix(team.train.x)

cvfit <- cv.glmnet(team.train.x, team.train.y, family = "binomial", type.measure = "class", nlambda = 1000)
plot(cvfit)
coef(cvfit, s = "lambda.min")

#Get training set predictions...We know they are biased but lets create ROC's.
#These are predicted probabilities from logistic model  exp(b)/(1+exp(b))
fit.pred <- predict(cvfit, newx = team.train.x, type = "response")

#Compare the prediction to the real outcome
head(fit.pred)
head(team.train.y)


#Create ROC curves
pred <- prediction(fit.pred[,1], team.train.y)
roc.perf = performance(pred, measure = "tpr", x.measure = "fpr")
auc.train <- performance(pred, measure = "auc")
auc.train <- auc.train@y.values

#Plot ROC
plot(roc.perf)
abline(a=0, b= 1) #Ref line indicating poor performance
text(x = .40, y = .6,paste("AUC = ", round(auc.train[[1]],3), sep = ""))

#Get Validation Set I
team.test.relevant <- team.test %>%
  dplyr::select("ps_team", "G", "win_pct", "R", "AB", "H", "X2B", "X3B", "HR", "BB", "SO", "SB", "CS", "RA", "ER", "ERA", "CG", "SHO", "SV", "IPouts", "HA", "HRA", "BBA", "SOA", "E", "DP", "FP", "attendance", "BPF", "PPF", "avg")

team.test.relevant <- team.test.relevant  %>% na.omit()

#select only numeric rows relevant to our investigation
team.test.x <- team.test.relevant  %>%
  dplyr::select("G", "win_pct", "R", "AB", "H", "X2B", "X3B", "HR", "BB", "SO", "SB", "CS", "RA", "ER", "ERA", "CG", "SHO", "SV", "IPouts", "HA", "HRA", "BBA", "SOA", "E", "DP", "FP", "attendance", "BPF", "PPF", "avg")
team.test.y <- team.test.relevant$ps_team
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

#lda
#select only numeric rows relevant to our investigation
team.train.x <- team.train.relevant  %>%
  dplyr::select("G", "win_pct", "R", "AB", "H", "X2B", "X3B", "HR", "BB", "SO", "SB", "CS", "RA", "ER", "ERA", "CG", "SHO", "SV", "IPouts", "HA", "HRA", "BBA", "SOA", "E", "DP", "FP", "attendance", "BPF", "PPF", "avg")

team.train.y <- team.train.relevant$ps_team
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
  dplyr::select("G", "win_pct", "R", "AB", "H", "X2B", "X3B", "HR", "BB", "SO", "SB", "CS", "RA", "ER", "ERA", "CG", "SHO", "SV", "IPouts", "HA", "HRA", "BBA", "SOA", "E", "DP", "FP", "attendance", "BPF", "PPF", "avg")
team.test.y <- team.test.relevant$ps_team
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


#Suppose we did not have all of these validation data sets.  We can assess how well our model building process works through Cross validation.
#The idea is that we can get an idea of how well the approach is going to perform on new data not yet collected.
#We will use AUC as the performance matrix.

nloops<-50   #number of CV loops
ntrains<-dim(team.train.x)[1]  #No. of samples in training data set
cv.aucs<-c() #initializing a vector to store the auc results for each CV run

for (i in 1:nloops){
  index<-sample(1:ntrains,60)
  cvtrain.x<-as.matrix(team.train.x[index,])
  cvtest.x<-as.matrix(team.train.x[-index,])
  cvtrain.y<-team.train.y[index]
  cvtest.y<-team.train.y[-index]
  
  cvfit <- cv.glmnet(cvtrain.x, cvtrain.y, family = "binomial", type.measure = "class") 
  fit.pred <- predict(cvfit, newx = cvtest.x, type = "response")
  pred <- prediction(fit.pred[,1], cvtest.y)
  roc.perf = performance(pred, measure = "tpr", x.measure = "fpr")
  auc.train <- performance(pred, measure = "auc")
  auc.train <- auc.train@y.values
  
  cv.aucs[i]<-auc.train[[1]]
}

hist(cv.aucs)
summary(cv.aucs)



#Doing the same procedure for random allocation of response values.
#Good practice when number of yes/no is not balanced.


nloops<-50   #number of CV loops
ntrains<-dim(team.train.x)[1]  #No. of samples in training data set
cv.aucs<-c()
team.train.yshuf<-team.train.y[sample(1:length(team.train.y))]

for (i in 1:nloops){
  index<-sample(1:ntrains,60)
  cvtrain.x<-as.matrix(team.train.x[index,])
  cvtest.x<-as.matrix(team.train.x[-index,])
  cvtrain.y<-team.train.yshuf[index]
  cvtest.y<-team.train.yshuf[-index]
  
  cvfit <- cv.glmnet(cvtrain.x, cvtrain.y, family = "binomial", type.measure = "class") 
  fit.pred <- predict(cvfit, newx = cvtest.x, type = "response")
  pred <- prediction(fit.pred[,1], cvtest.y)
  roc.perf = performance(pred, measure = "tpr", x.measure = "fpr")
  auc.train <- performance(pred, measure = "auc")
  auc.train <- auc.train@y.values
  
  cv.aucs[i]<-auc.train[[1]]
}

hist(cv.aucs)
summary(cv.aucs)

