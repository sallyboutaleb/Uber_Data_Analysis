# Uber Data Analysis

![uberdatapic](https://user-images.githubusercontent.com/118493723/234388524-54b06df0-b973-4bb2-81c3-24291b087194.png)

This project explores the data on Uber rides in different months and visualizes the patterns and trends in the data.


# Libraries 
 
```r
library(tidyverse)
library(ggplot2)
library(reshape2)
library(tidyr)
library(dplyr)
library(lubridate)
library(readxl)
library(tidytext)
library(leaflet)
library(ggthemes)
library(shiny)
```


# Data Dictionary 

The main data frame 'final_df' includes the following columns: 
 

Date: Date of the uber rides 
Time: time of the uber rides 
Lat: Latitude of the uber rides 
Lon: Longitude of the uber rides 
Base: the Base of the uber rides 
hour: time of the uber rides hour only (%H) 
month: The date of the uber rides expressed in month (non numerical)
da:y The day of the month of the uber rides 
weekday: day of the week of the uber rides 

# Data Preparation 

The script reads in six different CSV files containing data on Uber rides in different months and combines them into one data frame using rbind(). It then separates the Date.Time column into separate Date and Time columns and converts the Time column to a format that only shows hour and minute. It also creates a new column for the month, day, and weekday by using the functions format() and as.integer(). 

```r
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
# Create a new column for day and weekday for analysis
final_df$date <- strptime(as.character(final_df$Date), "%m/%d/%Y")
final_df$day <- strftime(final_df$date, "%d")
final_df$weekday <- as.integer(format(final_df$date, "%u")) 

```


### Create a pivot table to count trips by hour
I created a pivot table to count the number of trips by hour. It first groups the data by hour and then uses the summarise function to count the number of trips for each hour. Finally, it pivots the table using the pivot_longer function to make it easier to work with. 
```r
trips_by_hour <- final_df %>%
  group_by(hour) %>%
  summarise(trips = n()) %>%
  pivot_longer(cols = trips, names_to = "hour_type", values_to = "trip_count")
```


### Create a pivot table to count trips by hour and month
I created a pivot table to count the number of trips by hour and month. It first groups the data by both month and hour, then uses the summarise function to count the number of trips for each combination of month and hour. 
```r
trips_by_hour_month <- final_df %>%
  group_by(month, hour) %>%
  summarise(trips = n())
  ```
  
### Create a pivot table to count trips by month and base

 I created a pivot table to count the number of trips by month and base. It first groups the data by both month and base, then uses the summarise function to count the number of trips for each combination of month and base. 
   ```r
trips_by_hour_base <- final_df %>%
group_by(month, Base) %>%
summarise(trips = n()) 
 
```
 

### Create a pivot table to count trips EVERY day
I created a pivot table to count the number of trips every day. It first groups the data by both day and month, then uses the summarise function to count the number of trips for each combination of day and month.

  ```r
trips_everyday <- final_df %>%
  group_by(day, month) %>%
  summarise(trips = n())  
```  
 
 ### Heat Maps
 pivot table to visualize patterns in the number of trips over different days and hours using to display a by hour and day. The same logic for the rest of the heat maps. 
```r
day_hour <- final_df %>% 
  group_by(day, hour) %>% 
  summarize(trips = n())
``` 
#Leaflet map 

Identified the locations with the highest number of trips and counts the number of trips taken to each location, which I used for Geo Spatial leaflet application shiny app.
```r
leaflet_data <- final_df %>%
  group_by(Lat, Lon) %>%
  summarise(trips = n())

top_locations <- leaflet_data[order(-leaflet_data$trips), ] %>%
  head(n=100)
```

# Shiny App 

```r 
  titlePanel(title = "Uber Data"),
  
  tabPanel( "trips by hour", 
               div(style = "text-align: center;", 
               h1("Trips by hour"),
               p(""), 
               plotOutput('plot01', height = "675px") , 
               )
  )
  
  server <- function(input, output)  
  trips_by_hour_chart <- ggplot(trips_by_hour, aes(x = hour, y = trip_count)) +
      geom_bar(stat = "identity", color = "black", fill = "pink") +
      labs(title = "Trips by Hour",
           x = "hour",
           y = "Trip Count") 
          output$plot01 = renderPlot({trips_by_hour_chart})
```
