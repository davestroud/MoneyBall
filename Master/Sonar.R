
library(RCurl)
x <- getURL("https://raw.githubusercontent.com/selva86/datasets/master/Sonar.csv")
Sonar <- read.csv(text = x)


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