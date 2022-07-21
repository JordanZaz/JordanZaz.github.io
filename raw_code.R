## Installing and loading packages
install.packages("tidyverse")
install.packages("rmarkdown")
install.packages("janitor")
install.packages("skimr")
install.packages("reshape")
install.packages("here")
install.packages("ggpubr")
install.packages("knitr")
install.packages("readr")

## loading packages
library(tidyverse)
library(janitor)
library(skimr)
library(dplyr)
library(reshape)
library(ggplot2)
library(here)
library(lubridate)
library(ggpubr)
library(ggrepel)


## importing files
daily_activity <- read_csv("dailyActivity_merged.csv")
daily_calories <- read_csv("dailyCalories_merged.csv")
daily_steps <- read_csv("dailySteps_merged.csv")
sleep_date <- read_csv("sleepDay_merged.csv")
hourly_calories <- read_csv("hourlyCalories_merged.csv")
hourly_steps <- read_csv("hourlySteps_merged.csv")

## previewing data
head(daily_activity)
str(daily_activity)

head(daily_calories)
str(daily_calories)

head(daily_steps)
str(daily_steps)

head(sleep_date)
str(sleep_date)

head(hourly_calories)
str(hourly_calories)

head(hourly_steps)
str(hourly_steps)

## cleaning and formatting the data

## renaming column names to lower case for consistency
daily_activity <- rename_with(daily_activity, tolower)
daily_calories <- rename_with(daily_calories, tolower)
daily_steps <- rename_with(daily_steps, tolower)
hourly_calories <- rename_with(hourly_calories, tolower)
hourly_steps <- rename_with(hourly_steps, tolower)
sleep_date <- rename_with(sleep_date, tolower)

## checking for number of users
n_unique(daily_activity$id)
n_unique(daily_calories$id)
n_unique(daily_steps$id)
n_unique(hourly_calories$id)
n_unique(hourly_steps$id)
n_unique(sleep_date$id)

## now checking for duplicates
sum(duplicated(daily_activity))
sum(duplicated(daily_calories))
sum(duplicated(daily_steps))
sum(duplicated(hourly_calories))
sum(duplicated(hourly_steps))
sum(duplicated(sleep_date))

## removing duplicates and N/A values
daily_activity <- daily_activity %>%
  distinct() %>%
  drop_na()

daily_calories <- daily_calories %>% 
  distinct() %>% 
  drop_na()

daily_steps <- daily_steps %>% 
  distinct() %>% 
  drop_na()

hourly_calories <- hourly_calories %>% 
  distinct() %>% 
  drop_na()

hourly_steps <- hourly_steps %>% 
  distinct() %>% 
  drop_na()

sleep_date <- sleep_date %>% 
  distinct() %>% 
  drop_na()

## verifying that duplicates have been removed
sum(duplicated(daily_activity))
sum(duplicated(daily_calories))
sum(duplicated(daily_steps))
sum(duplicated(hourly_calories))
sum(duplicated(hourly_steps))
sum(duplicated(sleep_date))

## renaming date columns
daily_activity <- daily_activity %>% 
  dplyr::rename(date = activitydate)

daily_calories <- daily_calories %>% 
  dplyr::rename(date = activityday)

daily_steps <- daily_steps %>% 
  dplyr::rename(date = activityday)

hourly_calories <- hourly_calories %>% 
  dplyr::rename(date = activityhour)

hourly_steps <- hourly_steps %>% 
  dplyr::rename(date = activityhour)

sleep_date <- sleep_date %>% 
  dplyr::rename(date = sleepday)

## converting strings to date
daily_activity <- daily_activity %>% 
  mutate(date = as_date(date, format = "%m/%d/%Y"))

daily_calories <- daily_calories %>% 
  mutate(date = as_date(date, format = "%m/%d/%Y"))

daily_steps <- daily_steps %>% 
  mutate(date = as_date(date, format = "%m/%d/%Y"))

## the hourly data frames are now matching with datetimes

hourly_calories<- hourly_calories %>% 
  mutate(date = as.POSIXct(date,format ="%m/%d/%Y %I:%M:%S %p" , tz=Sys.timezone()))

hourly_steps<- hourly_steps %>% 
  mutate(date = as.POSIXct(date,format ="%m/%d/%Y %I:%M:%S %p" , tz=Sys.timezone()))

## will merge sleep_date with other daily data frames so will disregard the time
sleep_date <- sleep_date %>% 
  mutate(date = as_date(date, format = "%m/%d/%Y"))


## interesting summary statistics

## daily activity
daily_activity %>%  
  select(totalsteps,
         totaldistance,
         sedentaryminutes, calories) %>%
  summary()

daily_activity %>% 
  select(veryactiveminutes, fairlyactiveminutes, lightlyactiveminutes) %>% 
  summary()

## daily calories
daily_calories %>% 
  select(calories) %>% 
  summary()
## daily calories is the same as daily activity data frame, therefor no need for this data frame

## daily steps
daily_steps %>% 
  select(steptotal) %>% 
  summary()
## daily step total is the same as daily activity data frame total steps, there for no need for this data frame

## hourly calories
hourly_calories %>% 
  select(calories) %>% 
  summary()

## hourly steps
hourly_steps %>% 
  select(steptotal) %>% 
  summary()

## sleep date
sleep_date %>% 
  select(totalsleeprecords, totalminutesasleep, totaltimeinbed) %>% 
  summary()

## now left with 4 data frames ; daily_activity, hourly_calories, hourly_steps, sleep_date
## time to merge dataframes

## merging daily activity with sleep date by id and date
daily_activity_sleep_merged <- merge(daily_activity, sleep_date, by=c('id', 'date'))
head(daily_activity_sleep_merged)
glimpse(daily_activity_sleep_merged)

## merging hourly calories with hourly steps by id and date
hourly_merged <- merge(hourly_calories, hourly_steps, by=c('id', 'date'))
head(hourly_merged)
glimpse(hourly_merged)

## Viewing both merged data frames
View(daily_activity_sleep_merged)
View(hourly_merged)

## type of users
daily_average_users <- daily_activity_sleep_merged %>%
  group_by(id) %>%
  summarise (mean_daily_steps = mean(totalsteps), mean_daily_calories = mean(calories), mean_daily_sleep = mean(totalminutesasleep))
head(daily_average_users)

## classifying users
user_type_classified <- daily_average_users %>%
  mutate(user_type = case_when(
    mean_daily_steps < 5000 ~ "sedentary",
    mean_daily_steps >= 5000 & mean_daily_steps < 7499 ~ "lightly active", 
    mean_daily_steps >= 7500 & mean_daily_steps < 9999 ~ "fairly active", 
    mean_daily_steps >= 10000 ~ "very active"
  ))
head(user_type_classified)
glimpse(user_type_classified)

## percentage of each user type to better visualize them on a graph.
user_type_percent <- user_type_classified %>%
  group_by(user_type) %>%
  summarise(total = n()) %>%
  mutate(totals = sum(total)) %>%
  group_by(user_type) %>%
  summarise(total_percent = total / totals) %>%
  mutate(labels = scales::percent(total_percent))
user_type_percent$user_type_classified <- factor(user_type_percent$user_type , levels = c("very active", "fairly active", "lightly active", "sedentary"))
head(user_type_percent)



## Visualizations

## Total steps vs Total distance
ggplot(data = daily_activity)+
  geom_smooth(mapping = aes(x = totalsteps, y = totaldistance))+
  geom_point(mapping = aes(x = totalsteps, y = totaldistance))+
  labs(title = 'Total distance vs Total steps', x = 'Total steps', y = 'Distance traveled')
## the results show that total distance is clearly corelated to how many steps are taken

## Total Steps vs Calories
ggplot(data = daily_activity)+
  geom_smooth(mapping = aes(x = totalsteps, y = calories))+
  geom_point(mapping = aes(x = totalsteps, y = calories))+
  labs(title = 'Total Steps vs Total Calories', x = 'Total steps', y = 'Calories')
## Another result that shows a positive correlation from more steps taken equals more calories burned. Obviously

## Total Hours asleep vs sedentary Hours
ggplot(data = daily_activity_sleep_merged)+
  geom_point(mapping = aes(x = sedentaryminutes/60, y = totalminutesasleep/60))+
  geom_smooth(mapping = aes(x = sedentaryminutes/60, y = totalminutesasleep/60))+
  labs(title = 'Hours Asleep vs Sedentary Hours', x = 'Sedentary Hours', y = 'Hours Asleep')
## Shows that mid to low sedentary time relates to longer time asleep.

## Total Steps vs Hours Asleep
ggplot(data = daily_activity_sleep_merged)+
  geom_smooth(mapping = aes(x = totalsteps, y = totalminutesasleep/60))+
  geom_point(mapping = aes(x = totalsteps, y = totalminutesasleep/60))+
  labs(title = 'Total Steps vs Hours Asleep', x = 'Total Steps', y = 'Hours Asleep')
## Seems to be no correlation between how many steps you take to how many hours of sleep you get

## Mean daily steps vs mean daily sleep
ggplot(data = daily_average_users)+
  geom_smooth(mapping = aes(x = mean_daily_steps, y = mean_daily_sleep/60))+
  geom_point(mapping = aes(x = mean_daily_steps, y = mean_daily_sleep/60))+
  labs(title = 'Mean Daily Steps vs Mean Hours of Sleep ', x = 'Mean Steps', y = 'Mean Hours Asleep')
## Seems to show little to no correlation

## Mean daily steps vs mean daily calories
ggplot(data = daily_average_users)+
  geom_smooth(mapping = aes(x = mean_daily_steps, y = mean_daily_calories))+
  geom_point(mapping = aes(x = mean_daily_steps, y = mean_daily_calories))+
  labs(title = 'Mean Daily Steps vs Mean Daily Calories ', x = 'Mean Steps', y = 'Mean Daily Calories')
## The mean steps vs calories shows less of a correlation compared to the regular total chart

## Mean daily calories vs mean daily sleep
ggplot(data = daily_average_users)+
  geom_smooth(mapping = aes(x = mean_daily_calories, y = mean_daily_sleep/60))+
  geom_point(mapping = aes(x = mean_daily_calories, y = mean_daily_sleep/60))+
  labs(title = 'Mean Daily Calories vs Mean Hours of Sleep ', x = 'Mean Calories', y = 'Mean Hours Asleep')
## Seems to be no real correlation compared to mean calories burnt vs how many hours of sleep

## creating the average steps per hour
## need to merge hourly calories and hourly steps and split date and time
hourly_merged2.0 <- merge(hourly_steps, hourly_calories, by = c('id', 'date')) %>%
  mutate(day = format(as.POSIXct(hourly_steps$date,format="%m/%d/%Y %H:%M:%S %p"),"%m/%d/%Y"))%>%
  mutate(date = as.POSIXct(date,format = "%H:%M:%S"))%>%
  mutate(weekday = weekdays(as.Date(date)))
hourly_merged2.0$time <- format(hourly_merged2.0$date,"%H:%M:%S")
head(hourly_merged2.0)

## average steps each hour
hourly_merged2.0 %>%
  select(time, steptotal) %>%
  group_by(time) %>%
  summarize(averagesteps= mean(steptotal)) %>%
  ggplot()+
  geom_col(mapping = aes(x = time, y = averagesteps, fill = averagesteps))+
  scale_fill_gradient(low = "red", high = "green")+
  labs(title= ("Average Hourly Steps "), x='Time of day', y='Step count')+
  theme(axis.text.x =  element_text(angle = 45))
## Highest average steps are during lunch/midday and right after work.

## average calories each hour
hourly_merged2.0 %>%
  select(time, calories) %>%
  group_by(time) %>%
  summarize(averagecalories= mean(calories)) %>%
  ggplot()+
  geom_col(mapping = aes(x = time, y = averagecalories, fill = averagecalories))+
  scale_fill_gradient(low = "red", high = "green")+
  labs(title= ("Average Hourly Calories "), x='Time of day', y='Calories count')+
  theme(axis.text.x =  element_text(angle = 45))
## Highest average calories burned during the lunch/midday and right after work which correlates to the steps chart above

## pie chart of the classified users

## had to install the package "plotrix" then add it to my library for the 3D model
install.packages("plotrix")
library(plotrix)

## A 3D model of the user type distribution
x <- c(38, 21, 21, 21)
labels <-  c("Fairly Active","Lightly Active","Sedentary","Very Active")
piepercent<- round(100 * x / sum(x), 1)
pie3D(x, labels = piepercent,
      main = "User Type Distribution", col = rainbow(length(x)))
legend("topright", c("Fairly Active","Lightly Active","Sedentary","Very Active"),
       cex = 0.8, fill = rainbow(length(x)))
