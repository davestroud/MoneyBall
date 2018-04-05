library(readr)
library(tidyverse)

# Selected data sets loaded from Kaggle site
Master <- read_csv("Baseball_Index/Master.csv")
Batting <- read_csv("Baseball_Index/Batting.csv")
Pitching <- read_csv("Baseball_Index/Pitching.csv")
Fielding <- read_csv("Baseball_Index/Fielding.csv")
Teams <- read_csv("Baseball_Index/Teams.csv")
AllStar <- read_csv("Baseball_Index/AllstarFull.csv")
BattingPost <- read_csv("Baseball_Index/BattingPost.csv")

# Selected data sets from 2012 to 2016
Batting_df <-filter(Batting, yearID == 2012, 2013, 2014, 2015, 2016)
Pitching_df <- filter(Pitching, yearID == 2012, 2013, 2014, 2015, 2016)
Fielding_df <- filter(Fielding, yearID == 2012, 2013, 2014, 2015, 2016 )
Teams_df <- filter(Teams, yearID == 2012, 2013, 2014, 2015, 2016)
AllStar_df <- filter(AllStar, yearID == 2012, 2013, 2014, 2015, 2016)
BattingPost_df <- filter(BattingPost, yearID == 2012, 2013, 2014, 2015, 2016)

# View its class
class(Batting_df)

# View dimensions
dim(Batting_df)

# Look at column names
names(Batting_df)

# Look at data structure, dyplr way
glimpse(Batting_df)
