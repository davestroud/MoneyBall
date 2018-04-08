library(XML2R)
pre <- "http://gd2.mlb.com/components/game/mlb/year_2013/month_06/day_14/"
post <- c("gid_2013_06_14_phimlb_colmlb_1/inning/inning_all.xml",
          "gid_2013_06_14_seamlb_oakmlb_1/inning/inning_all.xml")
urls <- paste0(pre, post)
obs <- XML2Obs(urls, as.equiv=TRUE, quiet=TRUE)
table(names(obs))

