#Econ 294 Assignment 4#
#Part 1#
print("Alexa Wolford, 1445763")

#Load Data#
flights<-read.csv("https://raw.githubusercontent.com/EconomiCurtis/econ294_2015/master/data/flights.csv", stringsAsFactors=FALSE)
planes<-read.csv("https://raw.githubusercontent.com/EconomiCurtis/econ294_2015/master/data/planes.csv", stringsAsFactors=FALSE)
weather<-read.csv("https://raw.githubusercontent.com/EconomiCurtis/econ294_2015/master/data/weather.csv", stringsAsFactors=FALSE)
airports<-read.csv("https://raw.githubusercontent.com/EconomiCurtis/econ294_2015/master/data/airports.csv", stringsAsFactors=FALSE)

#Part 2: Convert Date Column#
library(dplyr)
flights <- flights %>% mutate(date = as.Date(date))
class(flights$date)

weather<- weather %>% mutate(date = as.Date(date))
class(weather$date)

#Part 3: Extract#
flights.2a<-subset(flights, (dest=="SFO" | dest=="OAK"))
print(nrow(flights.2a))

flights.2b<-subset(flights, dep_delay>=60)
print(nrow(flights.2b))

flights.2c<-subset(flights, arr_delay>2*dep_delay)
print(nrow(flights.2c))

#Part 4 Select #
library(dplyr)
?dplyr::select

select(flights, ends_with("delay"))
select(flights, contains("delay"))
select(flights, matches("delay"))

#Part 5 Arrange#
flights5<-arrange(flights, desc(dep_delay))
head(flights5$dep_delay, 5)

flights5b<-arrange(flights, arr_delay-dep_delay) 
head(flights5b, 5)

#Part 6 Mutate#
flights.6<- flights%>%
  mutate(hours = time/60,
       mph = dist/hours,
       delta = dep_delay - arr_delay)

#Part 7 View#
flights7a<-arrange(flights.6, desc(mph))
head(flights7a$mph, 5)

flights7b<-arrange(flights.6, desc(delta))
head(flights7b$delta, 5)

flights7c<-arrange(flights.6, (delta))
head(flights7c$delta, 5)

#Part 8 Group by and Summarize#
flights.8a <- flights %>%
  dplyr::group_by(carrier) %>%
  dplyr::summarise(
    cancel = sum(cancelled, na.rm=TRUE),
    cancel_percentage = 100*(cancel/nrow(flights)),
    min_delta = min(delta, na.rm=TRUE),
    delta_1 = quantile(delta,0.25, na.rm=TRUE),
    delta_median = median(delta, na.rm=TRUE),
    mean_delta = mean(delta, na.rm=TRUE),
    delta_3 = quantile(delta, 0.75, na.rm=TRUE),
    delta_90 = quantile(delta, 0.90, na.rm=TRUE),
    max_delta = max(delta, na.rm=TRUE),
    count = n()
  )

#Part 8 b
print("The code block creates a new data frame called day_delay by selecting/filtering the summary of flights, grouped by flights that that do not have an na value in departure delay column, the date, the mean departure delay time the n-count, and the observations after the 10th observation")

day_delay<-dplyr::filter(flights, !is.na(dep_delay))%>% group_by(date)%>%
      summarise(
        delay = mean(dep_delay),
        n=n())

#Part 8 Add new column to day_delay
?dplyr::lag
day_delay<-arrange(day_delay,desc(date))
delay_lag<-lag(day_delay$delay, 1, na.pad=TRUE)
cbind(day_delay$delay, delay_lag)

day_delay.8<-day_delay%>%
  mutate(avg_diff=delay_lag - delay)

day_delay.8<-arrange(day_delay.8, desc(avg_diff))
head(day_delay.8$avg_diff, 5, na.rm=TRUE)

#Part 9 Two Table Verbs Moving Options

dest_delay.9 <- flights %>%
  dplyr::group_by(dest) %>%
  dplyr::summarise(
    avg_arr_delay = mean(arr_delay, na.rm=TRUE),
    count = n()
  )

#Part 9 a
airports<- airports%>%
  rename(dest = iata, name = airport)

dest_delay.9a<-left_join(dest_delay.9, airports, by=c("dest"="dest"))
dest_delay.9a<-arrange(dest_delay.9a, desc(avg_arr_delay))
head(dest_delay.9a,5)
nrow(dest_delay.9a)
#Part 9 b
dest_delay.9b<-inner_join(dest_delay.9, airports, by=c("dest"="dest"))
dest_delay.9b<-arrange(dest_delay.9b, desc(avg_arr_delay))
head(dest_delay.9b,5)
nrow(dest_delay.9b)
print("The number of observations don't match. left_join gives 116 observations and inner_join gives 114")
#Part 9 c
dest_delay.9c<-right_join(dest_delay.9, airports, by=c("dest"="dest"))
dest_delay.9c<-arrange(dest_delay.9c, desc(avg_arr_delay))
head(dest_delay.9c,5)
nrow(dest_delay.9c)
Print("There are 3376 observations using the right_join. There are NA's in the arr_delay because we are joining the airports with the dest_delay, where airports has a different number of observations")
#Part 9 d
dest_delay.9d<-full_join(dest_delay.9, airports, by=c("dest"="dest"))
dest_delay.9d<-arrange(dest_delay.9d, desc(avg_arr_delay))
head(dest_delay.9d,5)
nrow(dest_delay.9d)
print("There are 3378 observations using full_join. There are NA's in the arr_delay because we are joining the airports with the dest_delay, where airports has a different number of observations")

#Part 10
hourly_delay<-flights %>%
  filter(flights, !is.na(dep_delay))%>%
  group_by(date, hour)%>%
  summarise(
    mean_dep_delays=mean(dep_delay,na.rm=TRUE), na.rm=TRUE
  )
  
hourly_delay$date<-as.Date(hourly_delay$date)
weather$date<-as.Date(weather$date)
hour.10<-left_join(hourly_delay,weather,by=c("date"="date"))



