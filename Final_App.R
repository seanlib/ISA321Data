library(tidyverse)
library(gapminder)
library(animation)
library(plotly)
library(crosstalk)
library(ggplot2)
library(ggmap)
library(stringr)
library(maps)
library(statebins)
library(maptools)
library(measurements)
library(shiny)
library(lpSolve)
library(shinythemes)
library(zoo)
#--------------------------------------------------------------------------------
#Reading in the data for the optimization part on tab 1
long_scores <- read.csv("long vectors3.csv")

cities <- c("Atlanta", "Austin", "Boston", "Chicago", "Cincinnati",
            "Cleveland", "Columbus", "Dallas", "Denver", "Houston",
            "Indianapolis", "Los Angeles", "Louisville", "Miami", "Milwaukee",
            "Minneapolis", "Nashville", "New Orleans", "New York", "Philadelphia",
            "Pittsburgh", "Portland, OR", "San Diego", "San Francisco", "Seattle")
cities2<-data.frame(cities)

#Adding in all data that is needed to build the map on tab 2
data1 <- read.csv("long vectors3.csv")
data1$Lat = gsub('°', ' ', data1$Lat)
data1$Long = gsub('°', ' ', data1$Long)
data1$Lat = measurements::conv_unit(data1$Lat, 
                                    from = 'deg_dec_min', 
                                    to = 'dec_deg')
data1$Long = measurements::conv_unit(data1$Long, 
                                     from = 'deg_dec_min', 
                                     to = 'dec_deg')
data1$Lat <- as.numeric(data1$Lat)
data1$Long <- as.numeric(data1$Long)

data1$Long <- data1$Long * -1

long_scores$Lat <- data1$Lat
long_scores$Long <- data1$Long

states.outlines <- map_data("state")
head(states.outlines)

state_data <- states.outlines %>%
  group_by(region) %>%
  nest()

states.stats <- read.csv("http://kmaurer.github.io/documents/data/StateStatsBRFSS.csv")

state_data <- left_join(state_data,states.stats, by=c("region"="StateName"))

states.all <- left_join(states.outlines, states.stats, by=c("region"="StateName"))

#--------------------------------------------------------------------------------
ui <- fluidPage(theme = shinytheme("superhero"),
                titlePanel(title = "Ideal Post Grad City"),
                sidebarLayout(
                  sidebarPanel(
                    actionButton(inputId = "Optimize", label = "Optimize City"),
                    actionButton(inputId = "Map", label = "Update Map"),
                    sliderInput(inputId ="MinTemp", label = "Min Temp",
                                min = 0, max = 49.99, value = 10),
                    sliderInput(inputId ="MaxTemp", label = "Max Temp",
                                min = 50, max = 100, value = 90),
                    sliderInput(inputId ="Weight1", label = "Unemployment Rate",
                                min = 0, max = 100, value = 0),
                    sliderInput(inputId ="Weight2", label = "Tax Rate",
                                min = 0, max = 100, value = 0),
                    sliderInput(inputId ="Weight3", label = "Crime Rate",
                                min = 0, max = 100, value = 0),
                    sliderInput(inputId ="Weight4", label = "Concert Venues",
                                min = 0, max = 100, value = 0),
                    sliderInput(inputId ="Weight5", label = "Breweries",
                                min = 0, max = 100, value = 0),
                    sliderInput(inputId ="Weight6", label = "Rent",
                                min = 0, max = 100, value = 0),
                    sliderInput(inputId ="Weight12", label = "Average Rainfall",
                                min = 0, max = 100, value = 0),
                    sliderInput(inputId ="Weight13", label = "Average Snowfall",
                                min = 0, max = 100, value = 0),
                    sliderInput(inputId ="Weight14", label = "Presence of Sports Teams",
                                min = 0, max = 100, value = 0),
                    sliderInput(inputId ="Weight15", label = "Public Parks",
                                min = 0, max = 100, value = 0),
                    sliderInput(inputId ="Weight16", label = "Total Population",
                                min = 0, max = 100, value = 0),
                    sliderInput(inputId ="Weight17", label = "Population (Age 25-34)",
                                min = 0, max = 100, value = 0),
                    sliderInput(inputId ="Weight18", label = "Percent of Millenial Population",
                                min = 0, max = 100, value = 0),
                    sliderInput(inputId ="Weight19", label = "Bar Density",
                                min = 0, max = 100, value = 0),
                    sliderInput(inputId ="Weight20", label = "Public Transportation",
                                min = 0, max = 100, value = 0)),
                  
                  #Create main panel to display the output
                  mainPanel(
                    tabsetPanel(id="tabs",
                                tabPanel("List", textOutput("myplot")),
                                tabPanel("Map", plotlyOutput("map")))
                  )
                )
)

#Defining server behavior
server <- function(input, output, session) {
  
  all_scores <- reactive({
    slider_min <- input$MinTemp
    slider_max <- input$MaxTemp
    
    # subsetting to remove cities that the user deems are too cold
    project_data <- long_scores
    jan_index <- seq(7, 487, 20)
    remove_city <- vector("numeric", 500)
    for (i in 1:nrow(project_data)) {
      if(project_data[i,3] < slider_min) {
        remove_city[i] <- project_data[i,1]
      } else {
        remove_city[i] <- NA
      }
    }
    
    remove_city <- remove_city[jan_index]
    remove_city <- remove_city[!is.na(remove_city)]
    
    test <- project_data[jan_index,]
    test <- test[remove_city,1]
    
    project_data <- subset(project_data, !(project_data$City %in% test))
    
    # Subsetting to remove cities the user deems are too hot
    project_data2 <- long_scores
    july_index <- seq(9, 489, 20)
    remove_city_hot <- vector("numeric", 500)
    for (j in 1:nrow(project_data2)) {
      if(project_data2[j,3] > slider_max) {
        remove_city_hot[j] <- project_data2[j,1]
      } else {
        remove_city_hot[i] <- NA
      }
    }
    
    remove_city_hot <- remove_city_hot[july_index]
    remove_city_hot <- remove_city_hot[!is.na(remove_city_hot)]
    
    test2 <- project_data2[july_index,]
    test2 <- test2[remove_city_hot,1]
    
    project_data2 <- subset(project_data2, !(project_data2$City %in% test2))
    
    # merging the data frame on an inner join
    long_scores <- merge(project_data,project_data2)
    long_scores <- long_scores[!long_scores$Variable == "AVG Temp (April)", ]
    long_scores <- long_scores[!long_scores$Variable == "AVG Temp (Jan)", ]
    long_scores <- long_scores[!long_scores$Variable == "AVG Temp (July)", ]
    long_scores <- long_scores[!long_scores$Variable == "AVG Temp (Oct)", ]
    long_scores <- long_scores[!long_scores$Variable == "Yearly AVG Temp", ]
  })
  
  weights <- reactive({
    weights <- c(
      input$Weight1, #Unemployment Rate        
      input$Weight2, #Tax rate                 
      input$Weight3, #Crime Rate              
      input$Weight4, #Number of Concert Venues 
      input$Weight5, #Number of Breweries
      input$Weight6, #median 1 BR rent        
      input$Weight12, #average rainfall        
      input$Weight13, #average snowfall 
      input$Weight14, #Presence of Sports Teams 
      input$Weight15, #Public Parks            
      input$Weight16, #Total Population         
      input$Weight17, #Age (25-34)              
      input$Weight18, #Population              
      input$Weight19, #Bar Density per 100k     
      input$Weight20 #Public Transportation
    )
  })
  
  observeEvent(input$Optimize, {
    
    updated_scores <- as.data.frame(all_scores())
    updated_weights <- weights()
    
    data2 <- updated_scores[,3] * updated_weights
    v <- data.frame(data2)
    
    n = 15
    combo1 <- rollapply(v, width=n, FUN=sum, by = n)
    combo2 <- data.frame(combo1)
    
    cities3 <- data.frame(unique(updated_scores[,1]))
    
    city_score <- cbind(cities3,combo2)
    
    obj.fun <- combo2
    # LHS <- rep(1:cities)
    LHS <- rep(1,nrow(cities3))
    signs <- "="
    RHS <- 1
    
    solution <- lp(direction = "min",
                   obj.fun,
                   LHS,
                   signs,
                   RHS,
                   all.bin=TRUE)

    IdealRow <- which.min(city_score[,2])
    
    IdealCity <- as.character(as.factor(city_score[IdealRow,1]), 
                              max.levels = 0)
    
    output$myplot <- renderPrint(IdealCity)
    })
  
  observeEvent(input$Map, {
    
    updated_scores <- as.data.frame(all_scores())
    updated_weights <- weights()
    
    data2 <- updated_scores[,3] * updated_weights
    v <- data.frame(data2)
    
    n = 15
    combo1 <- rollapply(v, width=n, FUN=sum, by = n)
    combo2 <- data.frame(combo1)
    
    redux1 <- updated_scores[seq(1, nrow(updated_scores), 15), ]
    
    cities3 <- data.frame(unique(updated_scores[,1]))
    
    city_score <- cbind(cities3,combo2)
    
    plot1 <- ggplot() + 
      geom_polygon(aes(x=long, y=lat, group=group), 
                   fill="lightgray",
                   data=unnest(state_data))+
      geom_path(aes(x=long,y=lat,
                    group=group),data=states.all)+
      geom_point(data = redux1, x = redux1[,5], 
                 y = redux1[,4], 
                 aes(color = city_score[,2],
                     text = paste("City:",city_score[,1],
                                  "Rank:",rank(city_score[,2]),
                                  sep = "\n")),
                 size = 2.75) +
      scale_colour_gradient(low="green", high="red",
                            guide = FALSE) +
      theme_minimal() +
      theme(panel.grid = element_blank(), axis.title = element_blank(),
            axis.text = element_blank())+
      coord_map()
  
  output$map <- renderPlotly({
    
    ggplotly(plot1, tooltip = c("text"))
    
  })
  })
}


shinyApp(ui=ui, server=server)


#Things to Get Done:

#1) Fix the map: Adjust so that the lat and long are being called from a subset data table
#2) Make the map reactive
#3) Add in subsetting based on temp to the optimization on tab 1
#4) Add in subsetting to the simulation model







