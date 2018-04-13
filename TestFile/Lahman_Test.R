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


# plot to show team wins and playoff appearance
ggplot(wildcard_era_teams, aes(teamID, W, color = Playoff)) + geom_point() + 
  theme(axis.text.x = element_text(angle = 45)) +
  xlab('Team') + ylab('Wins') + 
  geom_hline(yintercept = 94, linetype = 2) +
  ggtitle('Wins By Team Colored By Playoffs Appearances')


# summarize new data extracing out desired columns
team_wins <- wildcard_era_teams %>%
  group_by(teamID, yearID, lgID, divID, Playoff) %>%
  summarise(W = W) %>%
  ungroup() %>%
  arrange(teamID)

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

ggplot(wildcard_era_teams, aes(teamID, W)) + 
  geom_boxplot(fill = 'blue') + 
  geom_hline(yintercept = 94, linetype = 2, color = 'black') +
  geom_hline(yintercept = 88, linetype = 2, color = 'red') + 
  theme(axis.text.x = element_text(angle = 90)) +
  xlab('Team Name') + ylab('Wins') +
  ggtitle('Boxplots of Teams vs Wins')


# transform NA for sac flies and hit by pitches to O so can compute correlation matrix 
wildcard_era_teams$SF[is.na(wildcard_era_teams$SF)] <- 0
wildcard_era_teams$HBP[is.na(wildcard_era_teams$HBP)] <- 0
# new column for team Batting Average
wildcard_era_teams$BA <- wildcard_era_teams$H / wildcard_era_teams$AB
# new column for team On-Base Percentage
# this will return NA for seasone beofre HBP was actually recorded but this will not 
# affect the analysis
wildcard_era_teams <- transform(wildcard_era_teams, OBP = (H + BB + HBP) / 
                                  (AB + BB + HBP + SF))

# new column for team Slugging Percentage
wildcard_era_teams <- transform(wildcard_era_teams, SLG = (H + X2B + (2 * X3B) + 
                                                             (3 * HR)) / AB)
wildcard_era_teams <- transform(wildcard_era_teams, OPS = OBP + SLG)
# new column for run differential  
wildcard_era_teams$Diff <- wildcard_era_teams$R - wildcard_era_teams$RA


#subset same years for salary
wildcard_era_salaries <- salaries[salaries$yearID >= 1996, ]
# pick out desired columns to perform merge
team_salaries <- wildcard_era_salaries %>%
  group_by(teamID, yearID) %>%
  summarise(Salary = sum(salary)) %>%
  ungroup() %>%
  arrange(teamID)
wildcard_era_teams <- merge(wildcard_era_teams, team_salaries, by = c('teamID', 'yearID'))


set.seed(101)
split <- sample.split(wildcard_era_teams$Playoff, 0.8)
train <- subset(wildcard_era_teams, split == TRUE)
test <- subset(wildcard_era_teams, split == FALSE)
dim(train)

# subset data to only include numeric and integer variables
nums <- sapply(train, is.numeric) | sapply(train, is.integer)
# create data frame for correlation
train_nums <- train[,nums]
# exclude unwanted variables
train_nums <- train_nums[,-which(names(train_nums) %in% c('yearID', 'Rank', 'G', 'Ghome', 'IPouts', 'attendance', 'BPF', 'PPF'))]
#compute correlation matrix
corMatrix <- cor(train_nums)
# plot matrix
corrplot(corMatrix, method = "circle", type="lower", tl.cex=0.55, cl.cex = 0.5, add = FALSE, tl.pos="lower")
corrplot(corMatrix, method = "shade", type="upper", tl.cex=0.55, cl.cex = 0.5, add = TRUE, tl.pos="upper")


# plot wins vs runs
p1 <- ggplot(train, aes(W, R)) + geom_point(aes(color = Playoff)) + 
  xlab('Wins') + ylab('Runs') +
  ggtitle('Wins Per Runs\ncolored by Playoff Teams')
# plot wins vs runs given up
p2 <- ggplot(train, aes(W, RA)) + geom_point(aes(color = Playoff)) + 
  xlab('Wins') + ylab('Runs Against') +
  ggtitle('Wins Per Runs Against\n colored by Playoff Teams')

grid.arrange(p1, p2, ncol = 2)

ggplot(train, aes(W, Diff)) + geom_point(aes(color = Playoff)) + 
  xlab('Wins') + ylab('Difference in Runs (R - RA)') +
  ggtitle('Wins Per Runs Diffential\n colored by Playoff Teams')


p1 <- ggplot(train, aes(R, BA)) + 
  geom_point(aes(color = W)) + 
  scale_fill_discrete(name = 'Wins') +
  xlab('Runs') + ylab('Batting Average') +
  ggtitle(paste0('Runs by Batting Average\n Stratified by Wins\n Corelation: ', 
                 round(cor(train_nums$R, train_nums$BA), 3)))
p2 <- ggplot(train, aes(R, OBP)) + 
  geom_point(aes(color = W)) + 
  scale_fill_discrete(name = 'Wins') +
  xlab('Runs') + ylab('On Base Percentage') +
  ggtitle(paste0('Runs by On Base Percentage\n Stratified by Wins\n Corelation: ', 
                 round(cor(train_nums$R, train_nums$OBP), 3)))
p3 <- ggplot(train, aes(R, SLG)) + 
  geom_point(aes(color = W)) + 
  scale_fill_discrete(name = 'Wins') +
  xlab('Runs') + ylab('Slugging Percentage') +
  ggtitle(paste0('Runs by Slugging Percentage\n Stratified by Wins\n Corelation: ', 
                 round(cor(train_nums$R, train_nums$SLG), 3)))
p4 <- ggplot(train, aes(R, HR)) + 
  geom_point(aes(color = W)) + 
  scale_fill_discrete(name = 'Wins') +
  xlab('Runs') + ylab('Home Runs') +
  ggtitle(paste0('Runs by Home Runs\n Stratified by Wins\n Corelation: ', 
                 round(cor(train_nums$R, train_nums$HR), 3)))

grid.arrange(p1, p2, p3, p4, ncol = 2)


ggplot(train, aes(R, OPS)) + 
  geom_point(aes(color = W)) + 
  scale_fill_discrete(name = 'Wins') +
  xlab('Runs') + ylab('On Base Plus Slugging') +
  ggtitle(paste0('Runs by On Base Plus Slugging\n Stratified by Wins\n Corelation: ', 
                 round(cor(train_nums$R, train_nums$OPS), 3)))


p1 <- ggplot(train, aes(RA, ERA)) + 
  geom_point(aes(color = W)) + 
  scale_fill_discrete(name = 'Wins') +
  xlab('Runs Against') + ylab('Earned Run Average') +
  ggtitle(paste0('Runs Against by ERA\n Stratified by Wins\n Corelation: ', 
                 round(cor(train_nums$RA, train_nums$ERA), 3)))
p2 <- ggplot(train, aes(RA, SO)) + 
  geom_point(aes(color = W)) + 
  scale_fill_discrete(name = 'Wins') +
  xlab('Runs Against') + ylab('Strike Outs') +
  ggtitle(paste0('Runs Against by Strike Outs\n Stratified by Wins\n Corelation: ', 
                 round(cor(train_nums$RA, train_nums$SO), 3)))
p3 <- ggplot(train, aes(R, HA)) + 
  geom_point(aes(color = W)) + 
  scale_fill_discrete(name = 'Wins') +
  xlab('Runs Against') + ylab('Hits Against') +
  ggtitle(paste0('Runs Against by Hits Allowed\n Stratified by Wins\n Corelation: ', 
                 round(cor(train_nums$RA, train_nums$HA), 3)))
p4 <- ggplot(train, aes(RA, E)) + 
  geom_point(aes(color = W)) + 
  scale_fill_discrete(name = 'Wins') +
  xlab('Runs Against') + ylab('Errors') +
  ggtitle(paste0('Runs Against by Errors\n Stratified by Wins\n Corelation: ', 
                 round(cor(train_nums$RA, train_nums$E), 3)))

grid.arrange(p1, p2, p3, p4, ncol = 2)


ggplot(train, aes(W, Salary)) + geom_point(aes(color = Playoff)) +
  facet_wrap(~ yearID) + 
  xlab('Runs') + ylab('Team Salary') +
  ggtitle('Total Wins By Salary')

# Correlation vs salary
cor(train$W, train$Salary)


binary_train <- train
# change Playoff to 1 for Y and 0 for N to work with ROCR
binary_train$Playoff <- ifelse(binary_train$Playoff == 'Y', 1, 0)
# fit multiple
glmfit1 <- glm(Playoff ~ OPS, data = binary_train, family = binomial)
glmfit2 <- glm(Playoff ~ OPS + ERA, data = binary_train, family = binomial)
glmfit3 <- glm(Playoff ~ OPS + ERA + E, data = binary_train, family = binomial)
glmfit4 <- glm(Playoff ~ OPS + ERA + E + Salary, data = binary_train, family = binomial)
glmfit5 <- glm(Playoff ~ OPS + SF + SO + HA + RA + SV + BBA + DP + HRA + HR + 
                 AB + R + ER + X2B + H + SB + BB + HBP + SOA + E + SHO + OBP + Salary +
                 SLG + X3B + CG + BA + CS + OPS + ERA, data = binary_train, family = binomial)

with(train, table(Playoff))

EvalModelCF <- function(model) {
  pred <- predict(model, type = 'response')
  return(sum(diag(table(pred > 0.5, binary_train$Playoff))) / nrow(binary_train))      
}
EvalModelAUC <- function(model) {
  pred <- predict(model, type = 'response')
  ROCRpred <- prediction(pred, binary_train$Playoff)
  return(as.numeric(performance(ROCRpred, 'auc')@y.values))
}
# predict on training set CF
c(EvalModelCF(glmfit1), EvalModelCF(glmfit2), EvalModelCF(glmfit3),
  EvalModelCF(glmfit4), EvalModelCF(glmfit5))

# predict on training set AUC
c(EvalModelAUC(glmfit1), EvalModelAUC(glmfit2), EvalModelAUC(glmfit3),
  EvalModelAUC(glmfit4), EvalModelAUC(glmfit5))

# We can also try to use linear discrimatory analysis, and return
# a vector of the confusion matrix accuracy
ldafit1 <- lda(Playoff ~ OPS + ERA, data = binary_train)
ldafit2 <- lda(Playoff ~ OPS + ERA + BB + HA + BBA, data = binary_train)
Evallda <- function(model) {
  pred <- predict(model)
  lda.class <- pred$class
  return(sum(diag(table(lda.class, binary_train$Playoff)))/nrow(binary_train))
}
c(Evallda(ldafit1), Evallda(ldafit2))


set.seed(321)
numFolds <- trainControl(method = 'cv', number = 10)
cpGrid <- expand.grid(.cp = seq(0.001, 0.05, 0.001))
best_cp <- train(Playoff ~ ., data = binary_train, method = 'rpart', trControl = numFolds, tuneGrid = cpGrid)
CARTfit <- rpart(Playoff ~ OPS + SF + SO + HA + RA + SV + BBA + DP + HRA + HR + AB + R + ER + X2B + H + SB + BB + HBP + SOA + E + SHO + OBP + Salary + SLG + X3B + CG + BA + CS + OPS + ERA, data = binary_train, method = 'class', cp = best_cp$bestTune)
# plot tree  
prp(CARTfit)

# predict on training set
trainPredCART <- predict(CARTfit)
# prediction accuracy 
table(trainPredCART[,2] > 0.5, binary_train$Playoff)

sum(diag(table(trainPredCART[,2] > 0.5, binary_train$Playoff))) / nrow(binary_train)

# AUC 
ROCRpredCART <- prediction(trainPredCART[,2], binary_train$Playoff)
as.numeric(performance(ROCRpredCART, 'auc')@y.values)


# Gradient Boosting
set.seed(3)
boostfit <- gbm(Playoff ~ OPS + ERA + SF + SO + HA + BBA + DP + HRA + HR + AB + X2B + H + SB + BB + HBP + SOA + E + SHO + Salary + X3B + CG + BA + CS, data = binary_train, distribution="bernoulli", n.trees=5000, interaction.depth=4, shrinkage =0.2)
# training accuracy CF
boostpred <- predict(boostfit, n.trees = 5000)
table(boostpred > 0.5, binary_train$Playoff)

sum(diag(table(boostpred > 0.5, binary_train$Playoff)))/nrow(binary_train)

# AUC
ROCRpredBoost <- prediction(boostpred, binary_train$Playoff)
as.numeric(performance(ROCRpredBoost, 'auc')@y.values)

summary(boostfit)

# Update logistic regression
glmfit6 <- glm(Playoff ~ OPS + ERA + BB + HA + BBA, data = binary_train, family = binomial)
c(EvalModelCF(glmfit6), EvalModelAUC(glmfit6))

# View glmfit6 coefficients
glmfit6$coefficients

# For convenience we create new varibales to predict on our testing set.
best_model_glm <- glmfit6
best_model_lda <- ldafit1
best_model_tree <- CARTfit
best_model_boost <- boostfit

# Create binary test set
binary_test <- test
# change Playoff to 1 for Y and 0 for N
binary_test$Playoff <- ifelse(binary_test$Playoff == 'Y', 1, 0)

predTestglm <- predict(best_model_glm, newdata = binary_test, type = 'response')

table(predTestglm > 0.5, binary_test$Playoff)

sum(diag(table(predTestglm > 0.5, binary_test$Playoff))) / nrow(binary_test)

ROCRpredglm <- prediction(predTestglm, binary_test$Playoff)
as.numeric(performance(ROCRpredglm, 'auc')@y.values)

# LDA test predictions
predTestlda <- predict(best_model_lda, newdata = binary_test)

table(predTestlda$class, binary_test$Playoff)

sum(diag(table(predTestlda$class, binary_test$Playoff))) / nrow(binary_test)

