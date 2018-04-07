library(pitchRx)
dat <- scrape(start = "2013-06-01", end = "2013-06-01")

names(dat)


dim(dat[["pitch"]])

data(gids, package = "pitchRx")
head(gids)

MNaway13 <- gids[grep("2013_06_[0-9]{2}_minmlb*", gids)]
dat2 <- scrape(game.ids = MNaway13)

library(dplyr)
db <- src_sqlite("pitchfx.sqlite3", create = T)
scrape(start = "2008-01-01", end = Sys.Date(), connect = db$con)