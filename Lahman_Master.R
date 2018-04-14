library(tidyverse)
library(ggplot2)
library(caret)


# Baseball Zip File Url
url <- 'http://seanlahman.com/files/database/baseballdatabank-master_2018-03-28.zip'
# create a temp file
temp <- tempfile()
# fetch the file into the temp. file and download it
download.file(url, temp)
# extract the target file from temp. file
teams <- read.csv(unz(temp, 'Teams.csv'))
salaries <- read.csv(unz(temp, 'Salaries.csv'))


