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

Wrangled_Data <-
    Storm_dat %>%  
      select(EVTYPE, BGN_DATE, INJURIES, FATALITIES, PROPDMG, PROPDMGEXP, 
             CROPDMG, CROPDMGEXP) %>%
      mutate(BGN_DATE = dmy_hms(BGN_DATE),
             PROPDMGEXP = ifelse(PROPDMGEXP == 'K', 10^3, 
                                 ifelse(PROPDMGEXP == 'M', 10^6, 10^9)
                                 ),
             CROPDMGEXP = ifelse(CROPDMGEXP == 'K', 10^3, 
                                 ifelse(CROPDMGEXP == 'M', 10^6, 10^9)
                                 ),
             PROPDMG = PROPDMG*PROPDMGEXP,
             CROPDMG = CROPDMG*CROPDMGEXP,
             TOTAL_ECONOMICAL_LOSTS = CROPDMG+PROPDMG
             )  %>% 
      filter(year(BGN_DATE) %in% 2007:2011) 
# Plotting
## COSTS
### CROPS
Wrangled_Data %>% 
  mutate(EVTYPE = reorder(EVTYPE, PROPDMG, sum)) %>%
    ggplot(aes(y =EVTYPE, x= PROPDMG)) + 
       geom_bar(stat="identity", fill = 'darkred') + 
          ylab('') + xlab('') + 
          ggtitle('ECONOMICAL LOSTS IN PROPERTIES (USD) PER EVENT')
### CROPS
Wrangled_Data %>% 
  mutate(EVTYPE = reorder(EVTYPE, CROPDMG, sum)) %>%
    ggplot(aes(y =EVTYPE, x= CROPDMG)) + 
      geom_bar(stat="identity", fill = 'darkred') + 
        ylab('') + xlab('') + 
        ggtitle('ECONOMICAL LOSTS IN CROPS (USD) PER EVENT')

### TOTAL
Wrangled_Data %>% 
  mutate(EVTYPE = reorder(EVTYPE, TOTAL_ECONOMICAL_LOSTS, sum)) %>%
    ggplot(aes(y =EVTYPE, x= TOTAL_ECONOMICAL_LOSTS)) + 
       geom_bar(stat="identity", fill = 'darkred') + 
           ylab('') + xlab('') + 
           ggtitle('TOTAL ECONOMICAL LOSTS (USD) PER EVENT')
