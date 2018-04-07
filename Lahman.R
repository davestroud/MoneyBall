library(Lahman)

data(Batting)
head(Batting)
require("dplyr")

## Prelude: Extract information from Salaries and Master
## to be merged with the batting data.

# Subset of Salaries data
salaries <- Salaries %>%
  select(playerID, yearID, teamID, salary)

# Subset of Master table (player metadata)
masterInfo <- Master %>%
  select(playerID, birthYear, birthMonth, nameLast,
         nameFirst, bats)

# Left join salaries and masterInfo to batting data,
# create an age variable and sort by playerID, yearID and stint
# Returns an ignorable warning.
batting <- battingStats() %>% 
  left_join(salaries, 
            by =c("playerID", "yearID", "teamID")) %>%
  left_join(masterInfo, by = "playerID") %>%
  mutate(age = yearID - birthYear - 
           1L *(birthMonth >= 10)) %>%
  arrange(playerID, yearID, stint)

## Generate a ggplot similar to the NYT graph in the story about Ted
## Williams and the last .400 MLB season 
# http://www.nytimes.com/interactive/2011/09/18/sports/baseball/WILLIAMS-GRAPHIC.html

# Restrict the pool of eligible players to the years after 1899 and
# players with a minimum of 450 plate appearances (this covers the
# strike year of 1994 when Tony Gwynn hit .394 before play was suspended
# for the season - in a normal year, the minimum number of plate appearances is 502)

eligibleHitters <- batting %>%
  filter(yearID >= 1900 & PA > 450)

# Find the hitters with the highest BA in MLB each year (there are a
# few ties).  Include all players with BA > .400, whether they
# won a batting title or not, and add an indicator variable for
# .400 average in a season.

topHitters <- eligibleHitters %>%
  group_by(yearID) %>%
  filter(BA == max(BA)| BA >= .400) %>%
  mutate(ba400 = BA >= 0.400) %>%
  select(playerID, yearID, nameLast, 
         nameFirst, BA, ba400)

# Sub-data frame for the .400 hitters plus the outliers after 1950
# (averages above .380) - used to produce labels in the plot below
bignames <- topHitters %>%
  filter(ba400 | (yearID > 1950 & BA > 0.380)) %>%
  arrange(desc(BA))

# Variable to provide a vertical offset to certain
# labels in the ggplot below
bignames$yoffset <-  c(0, 0, 0, 0, 0.002, 0, 0, 0,
                       0.001, -0.001, 0, -0.002, 0, 0,
                       0.002, 0, 0)

# Produce the plot

require("ggplot2")                               
ggplot(topHitters, aes(x = yearID, y = BA)) +
  geom_point(aes(colour = ba400), size = 2.5) +
  geom_hline(yintercept = 0.400, size = 1, colour = "gray70") +
  geom_text(data = bignames, aes(y = BA + yoffset,
                                 label = nameLast), 
            size = 3, hjust = 1.2) +
  scale_colour_manual(values = c("FALSE" = "black", "TRUE" = "red")) +
  xlim(1899, 2015) +
  xlab("Year") +
  scale_y_continuous("Batting average",
                     limits = c(0.330, 0.430),
                     breaks = seq(0.34, 0.42, by = 0.02),
                     labels = c(".340", ".360", ".380", ".400", ".420")) +
  geom_smooth() +
  theme(legend.position = "none")

##########################################################
# after Chris Green,
# http://sabr.org/research/baseball-s-first-power-surge-home-runs-late-19th-century-major-leagues

# Total home runs by year
totalHR <- Batting %>%
  group_by(yearID) %>%
  summarise(HomeRuns = sum(as.numeric(HR), na.rm=TRUE),
            Games = sum(as.numeric(G), na.rm=TRUE))

# Plot HR by year, pre-1919 (dead ball era)
totalHR %>% filter(yearID <= 1918) %>%
  ggplot(., aes(x = yearID, y = HomeRuns)) +
  geom_line() +
  geom_point() +
  labs(x = "Year", y = "Home runs hit")

# Take games into account
totalHR %>% filter(yearID <= 1918) %>%
  ggplot(., aes(x = yearID, y = HomeRuns/Games)) +
  geom_line() +
  geom_point() +
  labs(x = "Year", y = "Home runs per game played")

# Widen perspective to all years from 1871
ggplot(totalHR, aes(x = yearID, y = HomeRuns)) +
  geom_point() +
  geom_path() +
  geom_smooth() +
  labs(x = "Year", y = "Home runs hit")

# Similar plot for HR per game played by year -
# shows several eras with spikes in HR hit
ggplot(totalHR, aes(x = yearID, y = HomeRuns/Games)) +
  geom_point() +
  geom_path() +
  geom_smooth(se = FALSE) +
  labs(x = "Year", y = "Home runs per game played")

##############################################################################

# some examples using dplyr with Lahman

library(Lahman)
library(dplyr)

#' ## Basic manipulations

# select some variables
batting <- select(tbl_df(Batting), playerID, yearID, teamID, G, AB:H, HR) 
# sort by player, year, team
batting <- arrange(batting, playerID, yearID, teamID)
# keep only recent years
batting <- filter(batting, yearID > 1985)

# add salary to Batting data; need to match by player, year and team
# NB:  dplyr coerces yearID to character because it is a factor in Salaries
(batting <- batting %>% left_join(Salaries))

# the same in base R using merge():
batting2 <- merge(batting, 
                  Salaries[,c("playerID", "yearID", "teamID", "salary")], 
                  by=c("playerID", "yearID", "teamID"), all.x=TRUE)


# Add name, age and bat hand information from Master
master <- select(tbl_df(Master), playerID, birthYear, birthMonth, 
                 nameLast, nameFirst, bats)
batting <- batting %>%
  left_join(master) %>%
  mutate(age = yearID - birthYear - ifelse(birthMonth < 10, 0, 1)) %>%
  select(-(birthYear:birthMonth))

# same with base R	                                 
Master[, c('playerID', 'birthYear', 'birthMonth',
           'nameLast', 'nameFirst', 'bats')]
batting2 <- merge(batting, masterInfo, all.x = TRUE)
batting2$age <- with(batting, yearID - birthYear -
                       ifelse(birthMonth < 10, 0, 1))

#' ## Queries about players

# form a players data.frame, that is grouped by playerID
players <- group_by(batting, playerID)

# For each player, find the two years with most hits
filter(players, min_rank(desc(H)) <= 2 & H > 0)

# Within each player, rank each year by the number of games played
mutate(players, G_rank = min_rank(G))

# For each player, find every year that was better than the previous year
filter(players, G > lag(G))
# For each player, compute avg change in games played per year
mutate(players, G_change = (G - lag(G)) / (yearID - lag(yearID)))

# For each player, find all where they played more games than average
filter(players, G > mean(G))
# For each, player compute a z score based on number of games played
mutate(players, G_z = (G - mean(G)) / sd(G))










