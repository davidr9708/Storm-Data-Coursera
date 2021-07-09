#Package
library(tidyverse)
library(R.utils)
library(lubridate)
# File used
##Downloading 
url <- 'https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2'
download.file(url, destfile = 'Raw_Data/StormData.csv.bz2')

## Decompressing
bunzip2(filename = 'Raw_Data/StormData.csv.bz2', 
        destname = 'Raw_Data/StormData.csv', remove = FALSE, skip = TRUE) 
## Reading
Storm_dat <- 
  read.csv('Raw_Data/StormData.csv')

# Wrangling  
## Taking only ye

Storm_dat %>%  select(CROPDMGEXP) %>% table() 
  filter(year(dmy_hms(BGN_DATE)) %in% 2007:2011, !is.na(CROPDMGEXP)) %>%
       select(PROPDMGEXP) %>% table() 

Storm_dat %>%  
  mutate(DBGN_DATE = dmy_hms(BGN_DATE)) 
  filter(DBGN_DATE %in% 2007:2011)
  select()       ) %>%
  select(EVTYPE) %>% table()

str(Storm_dat)

