library(XML2R)
pre <- "http://gd2.mlb.com/components/game/mlb/year_2013/month_06/day_14/"
post <- c("gid_2013_06_14_phimlb_colmlb_1/inning/inning_all.xml",
          "gid_2013_06_14_seamlb_oakmlb_1/inning/inning_all.xml")
urls <- paste0(pre, post)
obs <- XML2Obs(urls, as.equiv=TRUE, quiet=TRUE)
table(names(obs))

tmp <- re_name(obs, equiv=c("game//inning//top//atbat//pitch",
                            "game//inning//bottom//atbat//pitch"), diff.name="side")
tmp <- re_name(tmp, equiv=c("game//inning//top//atbat//runner",
                            "game//inning//bottom//atbat//runner"), diff.name="side")
tmp <- re_name(tmp, equiv=c("game//inning//top//atbat//po",
                            "game//inning//bottom//atbat//po"), diff.name="side")
tmp <- re_name(tmp, equiv=c("game//inning//top//atbat",
                            "game//inning//bottom//atbat"), diff.name="side")
obs2 <- re_name(tmp, equiv=c("game//inning//top//action",
                             "game//inning//bottom//action"), diff.name="side")
table(names(obs2))

obs2[grep("game//inning//atbat//po", names(obs2))][1:2]

obs2[grep("^game//inning$", names(obs2))][1:3]
