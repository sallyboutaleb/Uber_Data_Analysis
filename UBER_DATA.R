library(tidyverse)
library(ggplot2)
library(reshape2)
library(tidyr)
library(dplyr)
library(lubridate)
library(readxl)
library(tidytext)
library(stringr)
library(leaflet)
library(ggthemes)
library(treemap)
library(shiny)

rm(list = ls())
setwd("~/DATA 332/uber data")

df1<- read.csv("uber-raw-data-apr14.csv")
df2<- read.csv("uber-raw-data-may14.csv")
df3<- read.csv("uber-raw-data-jun14.csv")
df4<- read.csv("uber-raw-data-jul14.csv")
df5<- read.csv("uber-raw-data-aug14.csv")
df6<- read.csv("uber-raw-data-sep14.csv")

combined_df <- rbind(df1, df2, df3, df4, df5, df6)

final_df <- separate(combined_df, Date.Time , into = c("Date", "Time"), sep = " ")

# change the time format to hour and minute
final_df$Time <- format(as.POSIXct(final_df$Time, format = "%H:%M:%S"), format = "%H:%M")
final_df$hour <- format(as.POSIXct(final_df$Time, format = "%H:%M"), format = "%H")

# Create a new column for month only for analysis
final_df$month <- format(as.Date(final_df$Date, "%m/%d/%Y"), "%B")
# Create a new column for day 
final_df$date <- strptime(as.character(final_df$Date), "%m/%d/%Y")
final_df$day <- strftime(final_df$date, "%d")
final_df$weekday <- as.integer(format(final_df$date, "%u"))

# Create a pivot table to count trips by hour
trips_by_hour <- final_df %>%
  group_by(hour) %>%
  summarise(trips = n()) %>%
  pivot_longer(cols = trips, names_to = "hour_type", values_to = "trip_count")
#save as csv file
write.csv(trips_by_hour, "trips_by_hour.csv", row.names = FALSE)
#GRAPH 1

ggplot(trips_by_hour, aes(x = hour, y = trip_count)) +
  geom_bar(stat = "identity", color = "black", fill = "pink") +
  labs(title = "Trips by Hour",
       x = "hour",
       y = "Trip Count")

# Create a pivot table to count trips by month
trips_by_month <- final_df %>%
  group_by(month) %>%
  summarise(trips = n())
#save as csv file
write.csv(trips_by_month, "trips_by_month.csv", row.names = FALSE)

#GRAPH 2
ggplot(trips_by_month, aes(x = month, y = trips)) +
  geom_col(fill = "blue") +
  labs(title = "Total number of trips by month",
       x = "Month",
       y = "Number of trips")

# Create a pivot table to count trips by hour and month
trips_by_hour_month <- final_df %>%
  group_by(month, hour) %>%
  summarise(trips = n()) %>%
  ungroup()
#save as csv file
write.csv(trips_by_hour_month, "trips_by_hour_month.csv", row.names = FALSE)

#GRAPH 3
ggplot(trips_by_hour_month, aes(x = hour, y = trips, group = month, color = month)) +
  geom_line() +
  labs(title = "Trips by Hour and Month", x = "Hour of Day", y = "Number of Trips", color = "Month")


# Create a pivot table to count trips by month and base
trips_by_hour_base <- final_df %>%
group_by(month, Base) %>%
summarise(trips = n())
#save as csv file
write.csv(trips_by_hour_base, "trips_by_hour_base.csv", row.names = FALSE)

#GRAPH 4 
#Chart Trips by Bases and Month
ggplot(trips_by_hour_base, aes(x = Base, y = trips, fill = month)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Trips by Bases and Month",
       x = "Base",
       y = "Trips",
       fill = "Month") +
  theme_minimal()

# Create a pivot table to count trips EVERY day
trips_everyday <- final_df %>%
  group_by(day, month) %>%
  summarise(trips = n())

#save as csv file
write.csv(trips_everyday, "trips_everyday.csv", row.names = FALSE)

#GRAPH5
ggplot(trips_everyday, aes(x = day, y = trips, fill = factor(month))) +
  geom_col(position = "dodge") +
  labs(x = "Day of the Month", y = "Trips Taken",
       title = "Trips Taken Every Day of the Month") +
  scale_fill_discrete(name = "Month")
#Heat Maps
#1
day_hour <- final_df %>% 
  group_by(day, hour) %>% 
  summarize(trips = n())
#save as csv file
write.csv(day_hour, "day_hour.csv", row.names = FALSE)

ggplot(day_hour, aes(x = hour, y = day, fill = trips)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red") +
  labs(title = "Heat map by hour and day", x = "Hour of Day", y = "Day") 
#2
day_month <- final_df %>% 
  group_by(day, month) %>% 
  summarize(trips = n())
#save as csv file
write.csv(day_month, "day_month.csv", row.names = FALSE)

ggplot(day_month, aes(x = month, y = day, fill = trips)) +
  geom_tile() +
  scale_fill_gradient(low = "blue", high = "black") +
  labs(title = "Heat map by month and day", x = "Month", y = "Day of the month")
#3
weekday_base <- final_df %>% 
  group_by(weekday, Base) %>% 
  summarize(trips = n())
#save as csv file
write.csv(weekday_base, "weekday_base.csv", row.names = FALSE)

ggplot(weekday_base, aes(x = Base, y = weekday, fill = trips)) +
  geom_tile() +
  scale_fill_gradient(low = "yellow", high = "orange") +
  labs(title = "Heat map Bases and Day", x = "Base", y = "Day of the week")

#leaflet
leaflet_data <- final_df %>%
  group_by(Lat, Lon) %>%
  summarise(trips = n())

top_locations <- leaflet_data[order(-leaflet_data$trips), ] %>%
  head(n=100)

 #Prediction Model trips after 8pm 
ggplot(trips_by_hour_month, aes(month, trips)) +
  geom_point(data = filter(trips_by_hour_month, rank(hour) >= 20),
    size = 4, color = "white" )+
    geom_point(aes(colour = hour)
               )





