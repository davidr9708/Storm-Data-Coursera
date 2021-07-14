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
Wrangled_Data <-
    Storm_dat %>%  
      select(EVTYPE, BGN_DATE, INJURIES, FATALITIES, PROPDMG, PROPDMGEXP, 
             CROPDMG, CROPDMGEXP) %>%
      mutate(Year = year(dmy_hms(BGN_DATE)),
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
      filter(Year %in% 2010:2011) %>%
      mutate(Year = as.factor(Year))

save(Wrangled_Data,file = 'Rdat/Wrangled_Data.rda')

class(Wrangled_Data$Year)
# Plotting
## COSTS
### CROPS
Crops <-
  Wrangled_Data %>% 
    group_by(EVTYPE) %>%
    summarise(SUM = sum(CROPDMG), MEAN = mean(CROPDMG)) 

Crops%>%
  mutate(EVTYPE = reorder(EVTYPE, SUM, sum)) %>%  
  arrange(desc(SUM)) %>%
  filter(SUM > 0) %>%
      ggplot(aes(y =EVTYPE, x= SUM)) +
        geom_point(color = 'darkred') +
          ylab('') + xlab('') + 
          ggtitle('ECONOMICAL CROPS LOSTS (USD) BY EVENT') + scale_x_log10()

## PROPERTIES
Property <-
  Wrangled_Data %>% 
    group_by(EVTYPE) %>%
    summarise(SUM = sum(PROPDMG), MEAN = mean(PROPDMG)) 

Property %>%
  mutate(EVTYPE = reorder(EVTYPE, SUM, sum)) %>%  
  arrange(desc(SUM)) %>%
  filter(SUM > 0) %>%
      ggplot(aes(y =EVTYPE, x= SUM)) +
        geom_point(color = 'darkred') +
            ylab('') + xlab('') + 
            ggtitle('ECONOMICAL PROPERTIES LOSTS (USD) BY EVENT') + scale_x_log10()
## TOTAL
Total <-
  Wrangled_Data %>% 
    group_by(EVTYPE) %>%
    summarise(SUM = sum(TOTAL_ECONOMICAL_LOSTS), 
              MEAN = mean(TOTAL_ECONOMICAL_LOSTS)) 

Total %>%
  mutate(EVTYPE = reorder(EVTYPE, SUM, sum)) %>%  
  arrange(desc(SUM))  %>%
  filter(SUM > 0) %>%
    ggplot(aes(y =EVTYPE, x= SUM)) +
      geom_point(color = 'darkred') +
      ylab('') + xlab('') + 
      ggtitle('TOTAL ECONOMICAL LOSTS (USD) BY EVENT') + scale_x_log10()

## HEALTH
### FATALITIES
Fatalities <-
  Wrangled_Data %>% 
    group_by(EVTYPE) %>%
    summarise(SUM = sum(FATALITIES), MEAN = mean(FATALITIES)) 

Fatalities %>%
  mutate(EVTYPE = reorder(EVTYPE, SUM, sum)) %>%  
  arrange(desc(SUM)) %>%
  filter(SUM > 0) %>%
    ggplot(aes(y =EVTYPE, x= SUM)) +
      geom_point(color = 'darkred') +
      ylab('') + xlab('') + 
      ggtitle('DEATHS BY EVENT') 

Injuries <-
  Wrangled_Data %>% 
  group_by(EVTYPE) %>%
  summarise(SUM = sum(INJURIES), MEAN = mean(INJURIES)) 

Injuries %>%
  mutate(EVTYPE = reorder(EVTYPE, SUM, sum)) %>%  
  arrange(desc(SUM)) %>%
  filter(SUM > 0 ) %>%
    ggplot(aes(y =EVTYPE, x= SUM)) +
      geom_point(color = 'darkred') +
        ylab('') + xlab('') + 
        ggtitle('INJUREDS BY EVENT') + scale_x_log10()
