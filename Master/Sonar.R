library(tidyverse)
library(RCurl)
library(caret)
x <- getURL("https://raw.githubusercontent.com/selva86/datasets/master/Sonar.csv")
Sonar <- read.csv(text = x)

glimpse(Sonar)
summary(Sonar)

# Shuffle row indices: rows
rows <- sample(nrow(Sonar))

# Randomly order data: Sonar
Sonar <- Sonar[rows, ]

# Identify row to split on: split
split <- round(nrow(Sonar) * .60)

# Create train
train <- Sonar[1:split, ]

# Create test
test <- Sonar[(split + 1):nrow(Sonar), ]

# Fit glm model: model
model <- glm(Class ~ ., family = "binomial", train)

# Predict on test: p
p <- predict(model, test, type = "response")

# Calculate class probabilities: p_class
p_class <- ifelse(p > 0.50, 0,1 )


# https://stackoverflow.com/questions/19871043/r-package
# -caret-confusionmatrix-with-missing-categories
# overide to get confusion matrix to work
library(e1071)
u = union(p_class, test$Playoff)
t = table(factor(p_class, u), factor(test$Class, u))
confusionMatrix(t)






