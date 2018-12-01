library(lpSolve)
library(shinythemes)
library(shiny)
data <- read.csv("long vectors.csv")
ui <- fluidPage(theme = shinytheme("superhero"),
                titlePanel(title = "Ideal Post Grad City"),
                sidebarLayout(
                  sidebarPanel(
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
                    sliderInput(inputId ="Weight7", label = "AVG TEMP(Jan)",
                                min = 0, max = 100, value = 0),
                    sliderInput(inputId ="Weight8", label = "AVG TEMP(April)",
                                min = 0, max = 100, value = 0),
                    sliderInput(inputId ="Weight9", label = "AVG TEMP(July)",
                                min = 0, max = 100, value = 0),
                    sliderInput(inputId ="Weight10", label = "AVG TEMP(Oct)",
                                min = 0, max = 100, value = 0), 
                    sliderInput(inputId ="Weight11", label = "Yearly Average Temp",
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
                                min = 0, max = 100, value = 0),
                    actionButton(inputId = "Optimize", label = "Optimize City")),
                  
                  
                  #Create main panel to display the output
                  mainPanel(
                    textOutput("myplot")),
                )
)
#Defining server behavior
server <- function(input, output, session) {
  observeEvent(input$Optimize, {
    weights <- c(
      input$Weight1, #Unemployment Rate        
      input$Weight2, #Tax rate                 
      input$Weight3, #Crime Rate              
      input$Weight4, #Number of Concert Venues 
      input$Weight5, #Number of Breweries
      input$Weight6, #median 1 BR rent        
      input$Weight7, #AVG Temp (Jan)         
      input$Weight8, #AVG Temp (April)
      input$Weight9, #AVG Temp (July)         
      input$Weight10, #AVG Temp (Oct)  
      input$Weight11, #Yearly AVG Temp
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
    data2 <- data[,3]*weights
    data2 <- data.frame(data2)
    v = data2$data2
    n = 20
    combo1 <- unname(tapply(v, (seq_along(v)-1) %/% n, sum))
    combo2 <- data.frame(combo1)
    
    cities <- c("Atlanta",
                "Austin",
                "Boston",
                "Chicago",
                "Cincinnati",
                "Cleveland",
                "Columbus",
                "Dallas",
                "Denver",
                "Houston",
                "Indianapolis",
                "Los Angeles",
                "Louisville",
                "Miami",
                "Milwaukee",
                "Minneapolis",
                "Nashville",
                "New Orleans",
                "New York",
                "Philadelphia",
                "Pittsburgh",
                "Portland, OR",
                "San Diego",
                "San Francisco",
                "Seattle")
    cities2<-data.frame(cities)
    
    city_score <- cbind(cities2,combo2)
    
    obj.fun <- combo2
    
    # LHS <- rep(1:25)
    LHS <- rep(1,25)
    signs <- "="
    RHS <- 1
    solution <- lp(direction = "min",
                   obj.fun,
                   LHS,
                   signs,
                   RHS,
                   all.bin=TRUE)
    
    IdealCity <-as.character(print(as.factor(city_score[which.min(city_score$combo1),1]), max.levels = 0))
    output$myplot <- renderPrint(IdealCity)
    
  })
}


shinyApp(ui=ui, server=server)





