library(pitchRx)
library(tidyverse)
db <- src_sqlite("pitchfx.sqlite3", create = T)
scrape(start = "2012-01-01", end = Sys.Date(), connect = db$con)