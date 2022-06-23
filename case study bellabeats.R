library(tidyverse)
library(readxl)
library(dplyr) #dataframe transformation
library(readr)
library(skimr)
library(janitor)
library(readr)
library(magrittr)
library(ggplot2) #plotting
library(lubridate) #transforming date
library(ggpubr) #for pearson correlation
library(gapminder)
library(gganimate) #to animate graph
library(ggthemes) #to animate graph
library(transformr) #to animate graph
library(gifski) #to animate graph
library(sqldf) #to count number of unique values



################################################################################
#even before doing all above, understand the data first

#capturing all the datasets
files <- list.files(
  path = "D:\\User\\Courses\\Google Data Analytics coursera\\case study dataset",
  pattern  = ".csv",
  full.names = TRUE
)

files #check

#loop to store num of rows & cols
numrows <- c()
numcols <- c()
numunique <- c()

for (i in 1:18) {
  xzy <- read.csv(files[i])
  numrows <- append(numrows, nrow(xzy))
  numcols <- append(numcols, ncol(xzy))
  numunique <- append(numunique, n_unique(xzy$Id))
}



#remove path name from files
files <- list.files(
  path = "D:\\User\\Courses\\Google Data Analytics coursera\\case study dataset",
  pattern  = ".csv",
  full.names = FALSE
)

files #check

#loop to store excel
datasetlist <- c()

for (i in 1:18) {
  datasetlist <- append(datasetlist, files[i])
}

#dataframe to see dataset details
excelrowscols <- data.frame(datasets = datasetlist, rows = numrows, cols = numcols, numofparticipants = numunique)

excelrowscols[order(excelrowscols$rows),]



################################################################################
#cleaning the data

## reading the data first
daily_activities <- read_csv('D:\\User\\Courses\\Google Data Analytics coursera\\case study dataset\\dailyActivity_merged.csv')
sleep_log <- read_csv('D:\\User\\Courses\\Google Data Analytics coursera\\case study dataset\\sleepDay_merged.csv')
minmets <- read_csv('D:\\User\\Courses\\Google Data Analytics coursera\\case study dataset\\minuteMETsNarrow_merged.csv')

str(daily_activities)
str(sleep_log)
str(minmets)

sum(duplicated(daily_activities))
sum(duplicated(sleep_log))
sum(duplicated(minmets))


#proceed to remove duplicate and NA first

daily_activities <- daily_activities %>% distinct() %>% drop_na()

sleep_log <- sleep_log %>% distinct() %>% drop_na()

minmets <- minmets %>% distinct() %>% drop_na()


#check
sum(duplicated(daily_activities))
sum(duplicated(sleep_log))
sum(duplicated(minmets))

#then we proceed to fix the date and col names as we will be merging them later on

daily_activities <- daily_activities %>%
  rename(date = ActivityDate) %>%
  mutate(date = as.Date(date, format = "%m/%d/%Y"))

#format as date without time as all the time in sleep_log is at 12am
sleep_log <- sleep_log %>%
  rename(date = SleepDay) %>%
  mutate(date = as.Date(date, format = "%m/%d/%Y"))

minmets <- minmets %>%
  mutate(ActivityMinute = as.POSIXct(ActivityMinute, format = "%m/%d/%Y %I:%M:%S %p"))


#check
colnames(daily_activities)
class(daily_activities$date)
colnames(sleep_log)
class(sleep_log$date)
colnames(minmets)
class(minmets$ActivityMinute)


#proceed to merge the data
daily_activities_sleep <- merge(daily_activities, sleep_log)
#after merging, there wil only be left with 24 samples as sleepDay_merged only have 24 samples


#check
str(daily_activities_sleep)


#################################################################################
#analyze the data
#we will look at
# 1. User's activity level
# 2. Sleep behaviour

#user's activity level
daily_activities_sleep %>% 
  group_by(Id) %>% 
  summarise(avgsteps = mean(TotalSteps), avgcal = mean(Calories), addcalburn = avgcal - 1800) %>% 
  select(avgsteps, avgcal, addcalburn) %>% 
  summary(addcalburn)


daily_activities_sleep %>% 
  transform(Id = as.character(Id)) %>% 
  group_by(Id) %>% 
  summarise(avgsteps = mean(TotalSteps), avgcal = mean(Calories)) %>% 
  ggplot() +
  geom_col(aes(Id, avgsteps, fill = 'grey')) +
  geom_col(aes(Id, avgcal, fill = 'blue')) +
  geom_hline(yintercept = 10000, colour = 'black') + #10000 is the steps per day
  geom_hline(yintercept = 1800, colour = 'red', size = 0.8) + #1800 is the BMR calories
  scale_y_continuous(breaks = c(0, 1800, 5000, 10000, 15000, 20000)) +
  theme(axis.text.x = element_text(angle = 90)) +
  xlab('participant ID') +
  ylab('avg steps & calories burn per day') +
  scale_fill_manual(values = c('blue', 'grey'), name = 'Legend', labels = c('Calories', 'Steps'))



#we can see person's average SedentaryMinutes per day vs their very&fairly active minutes
daily_activities_sleep %>% 
  transform(Id = as.character(Id)) %>% 
  group_by(Id) %>% 
  summarise(avgsed = mean(SedentaryMinutes), avgactive = mean(VeryActiveMinutes+FairlyActiveMinutes)) %>% 
  ggplot() +
  geom_col(aes(Id, avgsed, fill = 'grey')) +
  geom_col(aes(Id, avgactive, fill = 'blue')) +
  theme(axis.text.x = element_text(angle = 90)) +
  xlab('participant ID') +
  ylab('avg Sedentary & Active mins per day') +
  scale_fill_manual(values = c('blue', 'grey'), name = 'Legend', labels = c('Active minutes', 'Sedentary Minutes'))

#then we just focus on a person's active level to see if they hit the WHO recommended 150mins moderate-intensity aerobic physical activity; 
daily_activities_sleep %>% 
  transform(Id = as.character(Id)) %>% 
  group_by(Id) %>% 
  summarise(avgactive = mean(VeryActiveMinutes+FairlyActiveMinutes)) %>% 
  ggplot() +
  geom_col(aes(Id, avgactive)) +
  geom_hline(yintercept = 30, colour = 'red') +
  scale_y_continuous(breaks = c(0, 30, 50, 100)) +
  theme(axis.text.x = element_text(angle = 90)) +
  xlab('participant ID') +
  ylab('avg active min per day')

#next we look at user's MET

#to split the chart, we find the ID of the that is positioned in middle of row
which(minmets$Id == "4445114986") #625861 so cut off at 625860 


minmets %>% 
  mutate(date = date(ActivityMinute)) %>%
  mutate(time = format(ActivityMinute, format = "%H%M")) %>%
  transform(time = as.numeric(time)) %>% 
  mutate(time = time/100) %>%
  relocate(METs, .after = time) %>% 
  filter(row_number() <= 625860) %>% 
  ggplot() +
  geom_line(aes(time, METs/10)) +
  ylab('MET') +
  xlab('Time') +
  ggtitle('Part 1') +
  geom_hline(yintercept = 6, colour = 'blue') +
  scale_x_continuous(breaks = c(0, 3, 6, 9, 12, 15, 18, 21)) +
  scale_y_continuous(breaks = c(0, 6, 10, 15)) +
  facet_wrap(~Id)

minmets %>% 
  mutate(date = date(ActivityMinute)) %>%
  mutate(time = format(ActivityMinute, format = "%H%M")) %>%
  transform(time = as.numeric(time)) %>% 
  mutate(time = time/100) %>%
  relocate(METs, .after = time) %>% 
  filter(row_number() >= 625861) %>% 
  ggplot() +
  geom_line(aes(time, METs/10)) +
  ylab('MET') +
  xlab('Time') +
  ggtitle('Part 2') +
  geom_hline(yintercept = 6, colour = 'blue') +
  scale_x_continuous(breaks = c(0, 3, 6, 9, 12, 15, 18, 21)) +
  scale_y_continuous(breaks = c(0, 6, 10, 15)) +
  facet_wrap(~Id)


#next we look at the MET readings at a day by day basis
graph1 <- minmets %>% 
  mutate(date = date(ActivityMinute)) %>%
  mutate(time = format(ActivityMinute, format = "%H%M")) %>%
  transform(time = as.numeric(time)) %>% 
  mutate(time = time/100) %>% 
  relocate(METs, .after = time) %>% 
  filter(Id == 1503960366) %>%
  ggplot() +
  geom_line(aes(time, METs/10)) +
  ylab('METs') +
  xlab('Time') +
  ggtitle('MET over days') +
  scale_x_continuous(breaks = c(0, 3, 6, 9, 12, 15, 18, 21)) +
  scale_y_continuous(breaks = c(0, 1, 6, 10, 15)) +
  geom_hline(yintercept = 6, colour = 'blue') +
  transition_time(date) +
  labs(subtitle = "Date: {frame_time}")

animate(graph1, height = 500, width = 1000, fps = 30, duration = 20, end_pause = 60, res = 100)


#next we look at how a person's amount of sedentary minutes affects their sleep hours

daily_activities_sleep %>% 
  ggplot(aes(SedentaryMinutes, TotalMinutesAsleep)) +
  geom_point() +
  geom_smooth() +
  geom_hline(yintercept = 420, colour = 'red') + #420mins = 7hrs sleep
  scale_y_continuous(breaks = c(0, 200, 400, 420, 600, 800)) +
  xlab('Total Sedentary Minutes') +
  ylab('Total Minutes Asleep') +
  stat_cor(method = "pearson") #-0.6


#next we look at how long a person take to sleep
#we will filter totalsleeprecord = 1, so we dont take into consideration naps etc
daily_activities_sleep %>% 
  transform(Id = as.character(Id)) %>% 
  mutate(timetosleep = TotalTimeInBed - TotalMinutesAsleep) %>% 
  filter(TotalSleepRecords == 1) %>% 
  ggplot(aes(Id, timetosleep)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90))

#we can see that there is 2 outlier to the dataset where they took around ~300mins & ~150mins on average
#to fall asleep

#we will proceed to remove those 2 samples and find the average time of them to fall asleep
daily_activities_sleep %>% 
  transform(Id = as.character(Id)) %>% 
  mutate(timetosleep = TotalTimeInBed - TotalMinutesAsleep) %>% 
  filter(TotalSleepRecords == 1, Id != '1844505072', Id != '3977333714') %>% 
  select(timetosleep) %>% 
  summary(timetosleep) #26.66172

daily_activities_sleep %>% 
  transform(Id = as.character(Id)) %>% 
  mutate(timetosleep = TotalTimeInBed - TotalMinutesAsleep) %>% 
  filter(TotalSleepRecords == 1, Id != '1844505072', Id != '3977333714') %>% 
  ggplot(aes(Id, timetosleep)) +
  geom_boxplot() +
  geom_hline(yintercept = 26.6, colour = 'red') +
  scale_y_continuous(breaks = c(0, 26.6, 50, 100, 150)) +
  xlab('participant ID') +
  ylab('time to sleep') +
  theme(axis.text.x = element_text(angle = 90))




#noted that some users have lesser days of data compared to the rest

#check for METS dataset
idlist <- unique(minmets$Id)

xminmets <- minmets %>% 
  mutate(date = date(ActivityMinute))

for (i in 1:33) {
  dates <- xminmets %>% 
    filter(Id == idlist[i]) %>% 
    distinct(date)
  
  print(paste('No.',i, 'ID', idlist[i], ', days of data recorded:', count(dates)))
}

#check for daily_activities_sleep dataset
idlist2 <- unique(daily_activities_sleep$Id)

for (i in 1:24) {
  dates <- daily_activities_sleep %>% 
    filter(Id == idlist2[i]) %>% 
    distinct(date)
  
  print(paste('No.',i, 'ID', idlist2[i], ', days of data recorded:', count(dates)))
}