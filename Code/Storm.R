#Package
library(tidyverse)
library(R.utils)
library(lubridate)
# File used
## 
url <- 'https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2'
download.file(url, destfile = 'Raw_Data/StormData.csv.bz2')

bunzip2(filename = 'Raw_Data/StormData.csv.bz2', 
        destname = 'Raw_Data/StormData.csv', remove = FALSE, skip = TRUE) 

Storm_dat <- 
  read_csv('Raw_Data/StormData.csv')

# Wrangling  
## Taking only year 2010
  table(month(dmy_hms(Storm_dat$BGN_DATE)),year(dmy_hms(Storm_dat$BGN_DATE)))
