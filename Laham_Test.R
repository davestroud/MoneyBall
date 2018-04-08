# Load Lahman library
library(Lahman)
library(tidyverse)

data(LahmanData)

# find ID variables in the datasets
IDvars <- lapply(LahmanData[,"file"], function(x) grep('.*ID$', colnames(get(x)), value=TRUE))
names(IDvars) <- LahmanData[,"file"]
str(IDvars)
# vector of unique ID variables
unique(unlist(IDvars))

# which datasets have playerID?
names(which(sapply(IDvars, function(x) "playerID" %in% x)))


players <- Master %>% 
  # Return one row for each distinct player
  distinct(playerID, nameFirst, nameLast)

players %>% 
  # Find all players who do not appear in Salaries
  anti_join(Salaries, by = "playerID") %>%
  # Count them
  count()

players %>% 
  anti_join(Salaries, by = "playerID") %>% 
  # How many unsalaried players appear in Appearances?
  semi_join(Appearances, by = "playerID") %>% 
  count()

players %>% 
  # Find all players who do not appear in Salaries
  anti_join(Salaries, by = "playerID") %>% 
  # Join them to Appearances
  left_join(Appearances, by = "playerID") %>%
  # Calculate total_games for each player
  group_by(playerID) %>% 
  summarize(total_games = sum(G_all, na.rm = TRUE)) %>%
  # Arrange in descending order by total_games
  arrange(desc(total_games))

players %>%
  # Find unsalaried players
  anti_join(Salaries, by = "playerID") %>% 
  # Join Batting to the unsalaried players
  left_join(Batting, by = "playerID") %>% 
  # Group by player
  group_by(playerID) %>% 
  # Sum at-bats for each player
  summarize(total_at_bat = sum(AB, na.rm = TRUE)) %>% 
  # Arrange in descending order
  arrange(desc(total_at_bat))


#Find the distinct players that appear in HallOfFame
nominated <- HallOfFame %>% 
  distinct(playerID)

nominated %>% 
  # Count the number of players in nominated
  count()

nominated_full <- nominated %>% 
  # Join to Master
  left_join(Master, by = "playerID") %>% 
  # Return playerID, nameFirst, nameLast
  select(playerID, nameFirst, nameLast)


# Find distinct players in HallOfFame with inducted == "Y"
inducted <- HallOfFame %>% 
  filter(inducted == "Y") %>% 
  distinct(playerID)

inducted %>% 
  # Count the number of players in inducted
  count()

inducted_full <- inducted %>% 
  # Join to Master
  left_join(Master, by = "playerID") %>% 
  # Return playerID, nameFirst, nameLast
  select(playerID, nameFirst, nameLast)

# Tally the number of awards in AwardsPlayers by playerID
nAwards <- AwardsPlayers %>% 
  group_by(playerID) %>% 
  tally()

nAwards %>% 
  # Filter to just the players in inducted 
  semi_join(inducted, by = "playerID") %>% 
  # Calculate the mean number of awards per player
  summarize(avg_n = mean(n, na.rm = TRUE))

nAwards %>% 
  # Filter to just the players in nominated 
  semi_join(nominated, by = "playerID") %>%
  # Filter to players NOT in inducted 
  anti_join(inducted, by = "playerID") %>%
  # Calculate the mean number of awards per player
  summarize(avg_n = mean(n, na.rm = TRUE))



# Find the players who are in nominated, but not inducted
notInducted <- nominated %>% 
  setdiff(inducted)

Salaries %>% 
  # Find the players who are in notInducted
  semi_join(notInducted, by = "playerID") %>%
  # Calculate the max salary by player
  group_by(playerID) %>% 
  summarize(max_salary = max(salary, na.rm = TRUE)) %>% 
  # Calculate the average of the max salaries
  summarize(avg_salary = mean(max_salary, na.rm = TRUE))

# Repeat for players who were inducted
Salaries %>% 
  semi_join(inducted, by = "playerID") %>% 
  group_by(playerID) %>% 
  summarize(max_salary = max(salary, na.rm = TRUE)) %>% 
  summarize(avg_salary = mean(max_salary, na.rm = TRUE))


Appearances %>% 
  # Filter Appearances against nominated
  semi_join(nominated, by = "playerID") %>% 
  # Find last year played by player
  group_by(playerID) %>% 
  summarize(last_year = max(yearID)) %>% 
  # Join to full HallOfFame
  left_join(HallOfFame, by = "playerID") %>% 
  # Filter for unusual observations
  filter(last_year >= yearID)

################################################
# Visualize relations among datasets via an MDS
################################################
# jaccard distance between two sets; assure positivity
jaccard <- function(A, B) {
  max(1 - length(intersect(A,B)) / length(union(A,B)), .00001)
}	

distmat <- function(vars, FUN=jaccard) {
  nv <- length(vars)
  d <- matrix(0, nv, nv, dimnames=list(names(vars), names(vars)))
  for(i in 1:nv) {
    for (j in 1:nv) {
      if (i != j) d[i,j] <- FUN(vars[[i]], vars[[j]])
    }
  }
  d
}

# do an MDS on distances
distID <- distmat(IDvars)
config <- cmdscale(distID)

pos=rep(1:4, length=nrow(config))
plot(config[,1], config[,2], xlab = "", ylab = "", asp = 1, axes=FALSE,
     main="MDS of ID variable distances of Lahman tables")
abline(h=0, v=0, col="gray80")
text(config[,1], config[,2], rownames(config), cex = 0.75, pos=pos, xpd=NA)



head(Master)

