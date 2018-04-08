library(pitchRx)
library(tidyverse)
db_2017 <- src_sqlite("pitchfx.sqlite3", create = T)
scrape(start = "2016-01-01", end = "2017-01-01", connect = db$con)

library(readr)
pitch_2018_04_07_16_48_20 <- read_csv("pitch-2018-04-07-16-48-20.csv")
View(pitch_2018_04_07_16_48_20)

atbat_2018_04_07_16_48_58 <- read_csv("atbat-2018-04-07-16-48-58.csv")
View(atbat_2018_04_07_16_48_58)

runner_2018_04_07_16_48_20 <- read_csv("runner-2018-04-07-16-48-20.csv")
View(runner_2018_04_07_16_48_20)

files <- c("miniscoreboard.xml", "players.xml", "inning/inning_hit.xml")
scrape(start = "2008-01-01", end = Sys.Date(), suffix = files, connect = db$con)


library(pitchRx)
dat <- scrape(start = "2013-06-01", end = "2013-06-01")

names(dat)

dim(dat[["pitch"]])

data(gids, package = "pitchRx")
head(gids)

MNaway13 <- gids[grep("2013_06_[0-9]{2}_minmlb*", gids)]
dat2 <- scrape(game.ids = MNaway13)


dat2$pitch

x <- list(
  facet_grid(pitcher_name ~ stand, labeller = label_both), 
  theme_bw(), 
  coord_equal()
)
animateFX(pitches, layer = x)

