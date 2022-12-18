
library(tidyverse)
library(magrittr)
library(plotly)
library(ggplot2)
library(leaflet)
library(shiny)
library(shinythemes)
library(leaflet)
library(rgdal)
library(readr)
library(geojsonio)

#modify stop for HRT
stop <- read_csv("stops.txt")
stop_HRT <- stop %>% filter (vehicle_type == "1")
RED_HRT <- stop_HRT[grep("Red Line",stop_HRT$`stop_desc`, ignore.case = T),]
BLUE_HRT <- stop_HRT[grep("Blue Line",stop_HRT$`stop_desc`, ignore.case = T),]
Orange_HRT <- stop_HRT[grep("Orange Line",stop_HRT$`stop_desc`, ignore.case = T),]

RED_HRT <- RED_HRT %>% mutate(traits = paste(from_stop_id, to_stop_id, sep = "~"))



HRT_year <- read.csv("HRT_Data.csv", header = TRUE)
HRT_year <- HRT_year %>% mutate(traits = paste(from_stop_id, to_stop_id, sep = "~"))

#Calculate the average travel times
HRT_avg_time <- HRT_year %>% group_by(traits)  %>% 
  summarise(AVG_time = round((mean(travel_time_sec))))

HRT_Trav <- HRT_avg_time %>% separate(col=`traits`,
                                     into = c("From", "To" ),
                                     sep = "~",
                                     fill = "right")

HRT_time <- cbind(HRT_avg_time, HRT_Trav[1:2])




#Draw map

spdf_file_bos <- geojson_read(  # Read the geojson file
  "Boston_Neighborhoods.geojson",
  what = "sp"
)
stats_df <- as.data.frame(spdf_file_bos)  # Export the census statistics in another data frame variable\

spdf_file_bos <- tidy(  # Convert it to a spatial data frame, with zip code as index
  spdf_file_bos,
  region="Neighborhood_ID"  # Use ZIPCODE variable as index, the index will be named "id"
)


ggplot() +
  geom_polygon(data=spdf_file_bos %>%
                 inner_join(stats_df, c("id"="Neighborhood_ID")),
               aes(x=long,
                   y=lat,
                   group=group,
                   fill=Acres),
               color="white",
               size=.2) +
  theme_void() +
  coord_map() +
  scale_fill_distiller(palette = "YlGnBu", direction = 1) +
  labs(title="Neighborhoods in Boston City",
       fill="Acres")


ggplot() +
  geom_polygon(data=spdf_file_bos,
               aes(x=long,
                   y=lat,
                   group=group),
               alpha=0,
               color="black",
               size=.2) +
  
  geom_point(data=RED_HRT,
             aes(x=stop_lon,
                 y=stop_lat),
             color="red",
             alpha=.6,
             size=3,
             shape=20) +
  
  geom_point(data=BLUE_HRT,
             aes(x=stop_lon,
                 y=stop_lat),
             color="blue",
             alpha=.6,
             size=3,
             shape=20) +
  
  geom_point(data=Orange_HRT,
             aes(x=stop_lon,
                 y=stop_lat),
             color="orange",
             alpha=.6,
             size=3,
             shape=20)+
  
  coord_map() +
  labs(title="HRT in Boston City")







#Define UI 
ui <- navbarPage(theme = shinytheme('lumen'), "MBTA HRT",
                 tabPanel("Query for Travel Times",
                          selectInput("dt",
                          label = "Start~End",
                          choices = HRT_avg_time$traits,
                          choices2 = HRT_avg_time$traits
                          ),
                          
                          dataTableOutput("Expected_Time"),
                          plotOutput("plot1"),
                          

                 )
)

                 




server <- function(input, output) {
  output$Expected_Time <- renderDataTable(HRT_avg_time$AVG_time[HRT_avg_time$traits == input$dt,], options = list(pageLength = 5))
  output$plot1 <- renderPlot({
    ggplot() +
      geom_polygon(data=spdf_file_bos,
                   aes(x=long,
                       y=lat,
                       group=group),
                   alpha=0,
                   color="black",
                   size=.2) +
      
      geom_point(data=RED_HRT,
                 aes(x=stop_lon,
                     y=stop_lat),
                 color="red",
                 alpha=.6,
                 size=3,
                 shape=20) +
      
      geom_point(data=BLUE_HRT,
                 aes(x=stop_lon,
                     y=stop_lat),
                 color="blue",
                 alpha=.6,
                 size=3,
                 shape=20) +
      
      geom_point(data=Orange_HRT,
                 aes(x=stop_lon,
                     y=stop_lat),
                 color="orange",
                 alpha=.6,
                 size=3,
                 shape=20)+
      
      coord_map() +
      labs(title="HRT in Boston City")
  })
  
  # output$plot2 <- renderPlot({
  #   ggplot() +
  #     geom_polygon(data=spdf_file_bos,
  #                  aes(x=long,
  #                      y=lat,
  #                      group=group),
  #                  alpha=0,
  #                  color="black",
  #                  size=.2) +
  #     geom_point(data=HRT_time[sick_ass$DISTRICT == input$dt,],
  #                aes(x=stop_lon,
  #                    y=stop_lat),
  #                color="orange",
  #                alpha=.6,
  #                size=3,
  #                shape=20)+
  
}

# Run the application 
shinyApp(ui = ui, server = server)
