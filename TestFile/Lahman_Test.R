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









