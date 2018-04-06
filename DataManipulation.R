library(dplyr)
library(hflights)

# View data
head(hflights)
summary(hflights)
glimpse(hflights)

# Turn data frame into tibble
hflights <-tbl_df(hflights)

# Make Carrier column easier to read.
lut <- c("AA" = "American", "AS" = "Alaska", "B6" = "JetBlue", "CO" = "Continental", 
         "DL" = "Delta", "OO" = "SkyWest", "UA" = "United", "US" = "US_Airways", 
         "WN" = "Southwest", "EV" = "Atlantic_Southeast", "F9" = "Frontier", 
         "FL" = "AirTran", "MQ" = "American_Eagle", "XE" = "ExpressJet", "YV" = "Mesa")

# Add the Carrier column to hflights
hflights$Carrier <- lut[hflights$UniqueCarrier]

#######################################################################
# The select verb

# Print out a tbl with the four columns of hflights related to delay
select(hflights, ActualElapsedTime, AirTime, ArrDelay, DepDelay)

# Print out the columns Origin up to Cancelled of hflights
select(hflights, 14:19)

# Find the most concise way to select: columns Year up to and including DayOfWeek, 
# columns ArrDelay up to and including Diverted. You can examine the order of the 
#variables in hflights with names(hflights) in the console.
select(hflights, 1:4, 12:21)

################################################################################
# Helper functions and variable selection

# Print out a tbl containing just ArrDelay and DepDelay
select(hflights, ends_with("Delay"))

# Use a combination of helper functions and variable names to print out only the 
# UniqueCarrier, FlightNum, TailNum, Cancelled, and CancellationCode columns of hflights.
select(hflights, UniqueCarrier, ends_with("Num"), starts_with("Cancel"))

# Find the most concise way to return the following columns with select and its helper 
# functions: DepTime, ArrTime, ActualElapsedTime, AirTime, ArrDelay, DepDelay.
select(hflights, contains("Tim"), contains("Del"))


############################################################################
# Comparison to base R

# Finish select call so that ex1d matches ex1r
ex1r <- hflights[c("TaxiIn", "TaxiOut", "Distance")]
ex1d <- select(hflights, contains("Taxi"), Distance)

# Finish select call so that ex2d matches ex2r
ex2r <- hflights[c("Year", "Month", "DayOfWeek", "DepTime", "ArrTime")]
ex2d <- select(hflights, Year:ArrTime, -DayofMonth)

# Finish select call so that ex3d matches ex3r
ex3r <- hflights[c("TailNum", "TaxiIn", "TaxiOut")]
ex3d <- select(hflights, starts_with("T"))

###############################################################################

# Mutation

# Add the new variable ActualGroundTime to a copy of hflights and save result as g1.
g1 <- mutate(hflights, ActualGroundTime = ActualElapsedTime - AirTime)

# Add the new variable GroundTime to g1. Save the result as g2.
g2 <- mutate(g1, GroundTime = TaxiIn + TaxiOut)

# Add the new variable AverageSpeed to g2. Save the result as g3.
g3 <- mutate(g2, AverageSpeed = Distance / AirTime * 60)

# Print out g3
g3

###############################################################################



