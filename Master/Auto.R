library(ISLR)
library(tidyverse)

dim(Auto)
glimpse(Auto)

newAuto<-Auto
#creating binary response for illustration
newAuto$mpg<-factor(ifelse(Auto$mpg>median(Auto$mpg),"High","Low"))
newAuto$cylinders<-factor(newAuto$cylinders)
newAuto$origin<-factor(newAuto$origin)


#Explore the data in various ways

#For summary stats, one of the most important things to do 
#is make note if certain categorical predictors are highly unbalanced
#including the response.
#If predictors are highly unbalanced, cross validation runs later could yield 
#some errors during the run.  You might have to resort to a test/train for model building or
#a manual CV.

#
#aggregate is good for summary stats by groups for continous predictors
aggregate(weight~mpg,data=newAuto,summary)
aggregate(displacement~mpg,data=newAuto,summary)

#lets attach newAuto so we don't have to keep writing newAuto$
attach(newAuto)
#Table of counts like proc freq are helpful for categorcal predictors
ftable(addmargins(table(mpg,cylinders))) 
#It probably is wise to throw out the 3 and 5 cylinder ones or combine it with 
#four or six.  I'll remove to keep it short.
newAuto<-newAuto[-which(cylinders %in% c(3,5)),]
attach(newAuto)
cylinders=factor(cylinders)
levels(cylinders)

ftable(addmargins(table(mpg,origin)))
ftable(addmargins(table(mpg,year))) 

#to get proportions that make sense
prop.table(table(mpg,cylinders),2)
prop.table(table(mpg,origin),2)
prop.table(table(mpg,year),2)

#Visualize
plot(mpg~cylinders,col=c("red","blue"))
plot(mpg~origin,col=c("red","blue"))
plot(mpg~year,col=c("red","blue"))

#Visualize
plot(weight~mpg,col=c("red","blue"))
plot(acceleration~mpg,col=c("red","blue"))
plot(displacement~mpg,col=c("red","blue"))

#Examine the correlation between the continous predictors
pairs(newAuto[,3:6])
my.cor<-cor(newAuto[,3:6])

#If you have a lot of predictors, heatmap with correlations could
#be helpful to examine redundancy.
library(gplots)
library(ggplot2)
heatmap.2(my.cor,col=redgreen(75), 
          density.info="none", trace="none", dendrogram=c("row"), 
          symm=F,symkey=T,symbreaks=T, scale="none")
#Note we don't scale here because we are dealing with correlations that are already
#scaled.
#Note above. You can also use the plots previously to examine one by one
#if predictors are associated category to categor or category to continuous.

#Another option here would be to do PCA among the continous predictors to see
#if they seperate out.  Or a heatmap.
pc.result<-prcomp(newAuto[,3:6],scale.=TRUE)
pc.scores<-pc.result$x
pc.scores<-data.frame(pc.scores)
pc.scores$mpg<-mpg

#Use ggplot2 to plot the first few pc's
ggplot(data = pc.scores, aes(x = PC1, y = PC2)) +
  geom_point(aes(col=mpg), size=1)+
  ggtitle("PCA of Auto")
#So we can see some pretty good seperation here.

#Here is where things get really personalized depending on the goal

#Prediction only
#With just one data set, to assess prediction preformance using Logistic and do feature selection
#properly, we should run some sort of CV.  We can either use LASSO to do this
#or the bestglm package which has forward selection techniques.
#The bestglm doesn't have a graphic unfortunately.  We could also write a script
#to do CV ourself or do a train/test split.

#The following code I'm to show you the CV as if ther is not test set
#You can rinse and repeat the prediction and ROC curves with a new data set
#easily.

library(glmnet)
library(bestglm)
dat.train.x <- model.matrix(mpg~cylinders+displacement+horsepower+weight+acceleration+year+origin-1,newAuto)
dat.train.y<-newAuto[,1]
library(glmnet)
cvfit <- cv.glmnet(dat.train.x, dat.train.y, family = "binomial", type.measure = "class", nlambda = 1000)
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

olog<-glm(mpg~cylinders+horsepower+weight+year+origin,data=newAuto,family=binomial)
fit.pred <- predict(olog, newx = dat.train.x, type = "response")

pred <- prediction(fit.pred[,1], dat.train.y)
roc.perf = performance(pred, measure = "tpr", x.measure = "fpr")
auc.train <- performance(pred, measure = "auc")
auc.train <- auc.train@y.values

#Plot ROC
plot(roc.perf,main="Ordingary Logistic")
abline(a=0, b= 1) #Ref line indicating poor performance
text(x = .40, y = .6,paste("AUC = ", round(auc.train[[1]],3), sep = ""))

#In this case they are not much different but they can be.
#glm or proc logistic will be the go to spot if you want inference
#and hypothesis testing

summary(olog)
plot(olog) #Residual plots but again, they are not very helpful.






