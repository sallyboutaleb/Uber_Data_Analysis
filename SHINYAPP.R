

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

setwd("~/DATA 332/UberShiny")


#read the data to csv files 
trips_by_hour <- read.csv("trips_by_hour.csv")
trips_by_month <- read.csv("trips_by_month.csv")
trips_by_hour_month <- read.csv("trips_by_hour_month.csv")
trips_by_hour_base <- read.csv("trips_by_hour_base.csv")
trips_everyday <- read.csv("trips_everyday.csv")
day_hour <- read.csv("day_hour.csv")
day_month <- read.csv("day_month.csv")
weekday_base <- read.csv("weekday_base.csv")
leaflet_data <- read.csv("top_locations.csv")

ui <- fluidPage(
  
  titlePanel(title = "Uber Data"),
  
  tabPanel( "trips by hour", 
               div(style = "text-align: center;", 
               h1("Trips by hour"),
               p("aa"), 
               plotOutput('plot01', height = "675px") , 
               )
  ), 

  tabPanel( "trips by month", 
            div(style = "text-align: center;", 
                h1("Trips by month"),
                p("aa"), 
                plotOutput('plot02', height = "675px") , 
            )
  ), 
  
  tabPanel( "trips by hour and month", 
            div(style = "text-align: center;", 
                h1("Trips by hour and month"),
                p("aa"), 
                plotOutput('plot03', height = "675px") , 
            )
  ), 
  
  
  tabPanel( "trips by hour and base", 
            div(style = "text-align: center;", 
                h1("Trips by hour and base"),
                p("aa"), 
                plotOutput('plot04', height = "675px") , 
            )
  ), 
  
  
  tabPanel( "trips everyday", 
            div(style = "text-align: center;", 
                h1("Trips everyday"),
                p("aa"), 
                plotOutput('plot05', height = "675px") , 
            )
  ),  

  
  tabPanel( "heatmap day and hour", 
            div(style = "text-align: center;", 
                h1("heatmap day and hour"),
                p("aa"), 
                plotOutput('plot06', height = "675px") , 
            )
  ), 
  
  tabPanel( "heatmap day and month", 
            div(style = "text-align: center;", 
                h1("heatmap day and month"),
                p("aa"), 
                plotOutput('plot07', height = "675px") , 
            )
  ),
  
  tabPanel( "heatmap weekday and base", 
            div(style = "text-align: center;", 
                h1("heatmap day and month"),
                p("aa"), 
                plotOutput('plot08', height = "675px") , 
            )
  ), 
  
  tabPanel( "leaflet map", 
            div(style = "text-align: center;", 
                h1("100 top locations"),
                p("aa"), 
                leafletOutput('plot09', height = "675px") , 
            )
  ),  
  
  tabPanel( "prediction model", 
            div(style = "text-align: center;", 
                h1("prediction model"),
                p("aa"), 
                plotOutput('plot010', height = "675px") , 
            )
  ), 
  
  
  
)


                        
server <- function(input, output){
  
  trips_by_hour_chart <- ggplot(trips_by_hour, aes(x = hour, y = trip_count)) +
      geom_bar(stat = "identity", color = "black", fill = "pink") +
      labs(title = "Trips by Hour",
           x = "hour",
           y = "Trip Count") 


trips_by_month_chart <- ggplot(trips_by_month, aes(x = month, y = trips)) +
  geom_col(fill = "blue") +
  labs(title = "Total number of trips by month",
       x = "Month",
       y = "Number of trips")


trips_by_hour_month_chart <- ggplot(trips_by_hour_month, aes(x = hour, y = trips, group = month, color = month)) +
  geom_line() +
  labs(title = "Trips by Hour and Month", x = "Hour of Day", y = "Number of Trips", color = "Month")


trips_by_hour_base_chart <- ggplot(trips_by_hour_base, aes(x = Base, y = trips, fill = month)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Trips by Bases and Month",
       x = "Base",
       y = "Trips",
       fill = "Month") +
  theme_minimal()


trips_everyday_chart <- ggplot(trips_everyday, aes(x = day, y = trips, fill = factor(month))) +
  geom_col(position = "dodge") +
  labs(x = "Day of the Month", y = "Trips Taken",
       title = "Trips Taken Every Day of the Month") +
  scale_fill_discrete(name = "Month")


day_hour_heatmap <- ggplot(day_hour, aes(x = hour, y = day, fill = trips)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red") +
  labs(title = "Heat map by hour and day", x = "Hour of Day", y = "Day") 


day_month_heatmap <- ggplot(day_month, aes(x = month, y = day, fill = trips)) +
  geom_tile() +
  scale_fill_gradient(low = "blue", high = "black") +
  labs(title = "Heat map by month and day", x = "Month", y = "Day of the month")


weekday_base_heatmap <- ggplot(weekday_base, aes(x = Base, y = weekday, fill = trips)) +
  geom_tile() +
  scale_fill_gradient(low = "yellow", high = "orange") +
  labs(title = "Heat map Bases and Day", x = "Base", y = "Day of the week")

top_location_leaflet <- leaflet(top_locations) %>% 
  addTiles() %>% 
  addMarkers(lng = top_locations$Lon, 
             lat = top_locations$Lat, 
             label = top_locations$trips)

prediction_model <- ggplot(trips_by_hour_month, aes(month, trips)) +
  geom_point(data = filter(trips_by_hour_month, rank(hour) >= 20),
             size = 4, color = "white" )+
  geom_point(aes(colour = hour)
  )


output$plot01 = renderPlot({trips_by_hour_chart})
output$plot02 = renderPlot({trips_by_month_chart})
output$plot03 = renderPlot({trips_by_hour_month_chart})
output$plot04 = renderPlot({trips_by_hour_base_chart})
output$plot05 = renderPlot({trips_everyday_chart})
output$plot06 = renderPlot({day_hour_heatmap})
output$plot07 = renderPlot({day_month_heatmap})
output$plot08 = renderPlot({weekday_base_heatmap})
output$plot09 = renderLeaflet({top_location_leaflet})
output$plot010 = renderPlot({prediction_model})

}

# Run the application 
shinyApp(ui = ui, server = server)














