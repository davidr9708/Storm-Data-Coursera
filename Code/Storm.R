#Package
library(tidyverse)
library(R.utils)
library(lubridate)
library(ggthemes)
library(ggpubr)
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
      filter(Year %in% 2007:2011) %>%
      mutate(Year = as.factor(Year))

save(Wrangled_Data,file = 'Rdat/Wrangled_Data.rda')

# Plotting
## COSTS
### DATA
Crops <-
  Wrangled_Data %>% 
    group_by(EVTYPE) %>%
    summarise(SUM = sum(CROPDMG), MEAN = mean(CROPDMG)) %>%
      mutate(EVTYPE = reorder(EVTYPE, SUM, sum)) %>%  
      arrange(desc(SUM)) %>%
      filter(SUM > 0) 

Property <-
  Wrangled_Data %>% 
    group_by(EVTYPE) %>%
    summarise(SUM = sum(PROPDMG), MEAN = mean(PROPDMG)) %>%
      mutate(EVTYPE = reorder(EVTYPE, SUM, sum)) %>%  
      arrange(desc(SUM)) %>%
      filter(SUM > 0) 

Total <-
  Wrangled_Data %>% 
    group_by(EVTYPE) %>%
    summarise(SUM = sum(TOTAL_ECONOMICAL_LOSTS), 
              MEAN = mean(TOTAL_ECONOMICAL_LOSTS))  %>%
      mutate(EVTYPE = reorder(EVTYPE, SUM, sum)) %>%  
      arrange(desc(SUM)) %>%
      filter(SUM > 0) 

### PLOTS
Crops_Plot <- 
  Crops%>%
    ggplot(aes(y =EVTYPE, x= SUM)) +
    geom_point(color = 'green4') +
       ylab('') + xlab('log10(Dollars)') + 
      labs(title = 'Crops Damage') + 
  scale_x_log10()+
  theme_update() 


Property_Plot <- 
  Property %>%
      ggplot(aes(y =EVTYPE, x= SUM)) +
        geom_point(color = 'tan4') +
            ylab('') + xlab('log10(Dollars)') + 
  labs(title = 'Property Damage') +  scale_x_log10()


Total_Plot <-
  Total %>%
    ggplot(aes(y =EVTYPE, x= SUM)) +
      geom_point(color = 'darkred') +
      labs(title = 'Total Damage') +
        ylab('') + xlab('log10(Dollars) ')  + scale_x_log10() 

##COMPLETED
Final_Plot <-
  ggarrange(Crops_Plot,Property_Plot, Total_Plot, ncol = 3)

annotate_figure(Final_Plot,
                top = text_grob('TOTAL ECONOMICAL DAMAGE BY EVENT \n(2007-2011)', face = "bold"))
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
      ggtitle('DEATHS BY EVENT') + scale_x_continuous(trans = 'log2')


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
        ggtitle('INJUREDS BY EVENT') + scale_x_continuous(trans = 'log2')
